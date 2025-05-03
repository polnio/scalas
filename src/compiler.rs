use crate::args::Args;
use crate::parser;
use convert_case::{Case, Casing as _};
use indexmap::IndexMap;
use slotmap::SlotMap;
use std::borrow::Cow;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::sync::LazyLock;

const REGISTERS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

static UNIT_TYPE: LazyLock<AppliedType> = LazyLock::new(|| AppliedType {
    type_: Type {
        generics_count: 0,
        cell_count_fn: |_| 0,
    },
    generics: Default::default(),
});
static NUMBER_TYPE: LazyLock<AppliedType> = LazyLock::new(|| AppliedType {
    type_: Type {
        generics_count: 0,
        cell_count_fn: |_| 1,
    },
    generics: Default::default(),
});
static STRING_TYPE: LazyLock<AppliedType> = LazyLock::new(|| AppliedType {
    type_: Type {
        generics_count: 0,
        cell_count_fn: |_| 2,
    },
    generics: Default::default(),
});

struct Variable {
    type_: AppliedType,
    offset: usize,
}

#[derive(Clone)]
struct Type {
    generics_count: usize,
    cell_count_fn: fn(&[AppliedType]) -> usize,
}
impl Type {
    fn cell_count(&self, generics: &[AppliedType]) -> usize {
        (self.cell_count_fn)(generics)
    }
}
#[derive(Clone)]
struct AppliedType {
    type_: Type,
    generics: Vec<AppliedType>,
}
impl AppliedType {
    fn cell_count(&self) -> usize {
        self.type_.cell_count(&self.generics)
    }
}
impl TryFrom<Type> for AppliedType {
    // TODO: Use a better error type
    type Error = ();
    fn try_from(value: Type) -> Result<Self, Self::Error> {
        if value.generics_count != 0 {
            return Err(());
        }
        Ok(Self {
            type_: value,
            generics: Default::default(),
        })
    }
}

struct Function {
    args: Vec<AppliedType>,
    ret: AppliedType,
}

type EnvKey = slotmap::DefaultKey;

#[derive(Default)]
struct Env {
    scope: Option<String>,
    parent: Option<EnvKey>,
    variables: IndexMap<String, Variable>,
    functions: IndexMap<String, Function>,
    types: IndexMap<String, Type>,
    // children: Vec<EnvKey>,
}
impl Env {
    fn new() -> Self {
        Self::default()
    }
    fn with_scope(scope: String, parent: EnvKey) -> Self {
        Self {
            scope: Some(scope),
            parent: Some(parent),
            ..Default::default()
        }
    }
}

macro_rules! impl_get_set {
    ($name:ident : $ty:ty) => {
        paste::paste! {
            #[allow(unused)]
            fn [<get_ $name s>](&self, env: EnvKey) -> impl Iterator<Item = (&String, &$ty)> {
                let env = self.envs.get(env).unwrap();
                env.[<$name s>].iter()
            }
            #[allow(unused)]
            fn [<get_ $name>](&self, ident: &str, env: EnvKey) -> Option<&$ty> {
                let env = self.envs.get(env).unwrap();
                if let Some(value) = env.[<$name s>].get(ident) {
                    return Some(value);
                }
                if let Some(parent) = env.parent {
                    return self.[<get_ $name>](ident, parent);
                }
                None
            }
            #[allow(unused)]
            fn [<insert_ $name>](&mut self, ident: String, value: $ty, env: EnvKey) {
                let env = self.envs.get_mut(env).unwrap();
                env.[<$name s>].insert(ident, value);
            }
        }
    };
}
struct Compiler<W: Write> {
    w: W,
    current_env: EnvKey,
    envs: SlotMap<EnvKey, Env>,
    data: Vec<String>,
}
impl<W: Write> Compiler<W> {
    pub fn new(w: W) -> Self {
        let mut envs = SlotMap::with_key();
        let current_env = envs.insert(Default::default());
        let data = Default::default();
        Self {
            w,
            current_env,
            envs,
            data,
        }
    }
    pub fn subenv(&mut self, ident: String, env: EnvKey) -> EnvKey {
        self.envs.insert(Env::with_scope(ident, env))
    }
    fn convert_type(&self, type_: &parser::Type) -> AppliedType {
        AppliedType {
            type_: self
                .get_type(&type_.ident, self.current_env)
                .unwrap()
                .clone(),
            generics: type_
                .generics
                .iter()
                .map(|type_| self.convert_type(type_))
                .collect(),
        }
    }
    fn expr_type(&self, expr: &parser::Expr) -> &AppliedType {
        match expr {
            parser::Expr::Literal(l) => match l {
                parser::Literal::Number(_) => &NUMBER_TYPE,
                parser::Literal::String(_) => &STRING_TYPE,
            },
            parser::Expr::FnCall(fn_call) => {
                let fn_ = self.expr_type(&parser::Expr::Ident(fn_call.ident.clone()));
                // Expect Fn[Args, Ret]
                let type_ = fn_.generics.first().unwrap();
                type_
            }
            parser::Expr::Ident(ident) => {
                let variable = self.get_variable(ident, self.current_env).unwrap();
                &variable.type_
            }
        }
    }
    fn scope(&self, env: EnvKey) -> Option<String> {
        let env = self.envs.get(env).unwrap();
        let this = env.scope.as_ref();
        let parent = env.parent.and_then(|parent| self.scope(parent));
        match (this, parent) {
            (Some(this), Some(parent)) => Some(format!("{}.{}", parent, this)),
            (Some(this), None) => Some(this.clone()),
            (None, Some(parent)) => Some(parent),
            (None, None) => None,
        }
    }
    fn scoped<'a>(&'a self, ident: &'a str, env: EnvKey) -> Cow<'a, str> {
        match self.scope(env) {
            Some(scope) => format!("{}.{}", scope, ident).into(),
            None => ident.into(),
        }
    }
    fn scoped_ident<'a>(&'a self, ident: &'a str, env: EnvKey) -> Option<Cow<'a, str>> {
        let e = self.envs.get(env).unwrap();
        if e.variables.get(ident).is_some() {
            Some(self.scoped(ident, env))
        } else if e.functions.get(ident).is_some() {
            Some(self.scoped(ident, env))
        } else if e.types.get(ident).is_some() {
            Some(self.scoped(ident, env))
        } else if let Some(parent) = e.parent {
            self.scoped_ident(ident, parent)
        } else {
            None
        }
    }
    fn create_string(&mut self, s: &str) -> std::io::Result<()> {
        let n = self.data.len();
        let name = format!("data.{}", n);
        write!(
            self.w,
            "  sub rsp, 16\n  mov qword [rsp], {}\n  mov qword [rsp+8], {}\n",
            name,
            s.len()
        )?;
        self.data.push(format!("db \"{}\"", s));
        Ok(())
    }
    impl_get_set!(variable: Variable);
    impl_get_set!(function: Function);
    impl_get_set!(type: Type);
    fn compile(&mut self, program: parser::Program, path: &Path) -> std::io::Result<()> {
        let obj_name = path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_case(Case::Pascal);
        write!(
            self.w,
            "format ELF64 executable\n_start:\n  push rbp\n  mov rbp, rsp\n  xor r8, r8\n.loop:\n  cmp r8, [rbp+8]\n  je .done\n  inc r8\n  sub rsp, 16\n  mov rdi, [rbp+r8*8+8]\n  mov qword [rsp], rdi\n  call internals.strlen\n  mov qword [rsp+8], rax\n  jmp .loop\n.done:\n  sub rsp, 16\n  mov rax, [rsp+16]\n  mov [rsp], rax\n  mov [rsp+8], r8\n  mov rdi, rsp\n  call {}.main\n  mov rsp, rbp\n  pop rbp\n  mov rax, 60\n  xor rdi, rdi\n  syscall\n",
            obj_name
        )?;

        self.insert_type("Unit".to_owned(), UNIT_TYPE.type_.clone(), self.current_env);
        self.insert_type(
            "Number".to_owned(),
            NUMBER_TYPE.type_.clone(),
            self.current_env,
        );
        self.insert_type(
            "String".to_owned(),
            STRING_TYPE.type_.clone(),
            self.current_env,
        );
        self.insert_type(
            "Array".to_owned(),
            Type {
                generics_count: 1,
                cell_count_fn: |_| 2,
            },
            self.current_env,
        );
        self.insert_type(
            "Fn".to_owned(),
            Type {
                generics_count: 2,
                cell_count_fn: |_| 1,
            },
            self.current_env,
        );

        self.insert_function(
            "print".to_owned(),
            Function {
                args: vec![STRING_TYPE.clone()],
                ret: UNIT_TYPE.clone(),
            },
            self.current_env,
        );
        self.insert_function(
            "println".to_owned(),
            Function {
                args: vec![STRING_TYPE.clone()],
                ret: UNIT_TYPE.clone(),
            },
            self.current_env,
        );

        let objects_envs = program
            .objects
            .iter()
            .map(|object| {
                let object_env = self.subenv(object.ident.to_string(), self.current_env);
                object
                    .fns
                    .iter()
                    .map(|fn_| {
                        self.insert_function(
                            fn_.ident.to_string(),
                            Function {
                                args: fn_
                                    .args
                                    .iter()
                                    .map(|(_ident, type_)| self.convert_type(type_))
                                    .collect(),
                                ret: self.convert_type(&fn_.ret),
                            },
                            object_env,
                        );
                        let fn_env = self.subenv(fn_.ident.to_string(), object_env);
                        (object_env, fn_env)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        for (object, envs) in program.objects.iter().zip(objects_envs) {
            for (fn_, (_object_env, fn_env)) in object.fns.iter().zip(envs) {
                write!(
                    self.w,
                    "{}.{}:\n  push rbp\n  mov rbp, rsp\n",
                    *object.ident, *fn_.ident
                )?;
                let parent_env = self.current_env;
                self.current_env = fn_env;
                for (i, (ident, type_)) in fn_.args.iter().enumerate() {
                    write!(self.w, "  mov rax, {}\n", REGISTERS[i])?;
                    self.generate_value_assign(ident.to_string(), self.convert_type(type_))?;
                }
                for stmt in &fn_.body {
                    self.compile_stmt(stmt)?;
                }
                write!(self.w, "  mov rsp, rbp\n  pop rbp\n  ret\n")?;
                self.current_env = parent_env;
            }
        }

        write!(self.w, "exit:\n  mov rax, 60\n  syscall\n  ret\n")?;
        write!(
            self.w,
            "print:\n  mov rsi, [rdi]\n  mov rdx, [rdi+8]\n  mov rax, 1\n  mov rdi, 1\n  syscall\n  ret\n"
        )?;
        write!(
            self.w,
            "println:\n  call print\n  mov rax, 1\n  mov rdi, 1\n  mov rsi, data.newline\n  mov rdx, 1\n  syscall\n  ret\n"
        )?;
        write!(
            self.w,
            "internals.strlen:\n  xor rax,rax\n.loop:\n  cmp byte [rdi], 0\n  je .done\n  inc rax\n  inc rdi\n  jmp .loop\n.done:\n  ret\n"
        )?;

        write!(self.w, "data.newline: db 10\n")?;
        for (i, data) in self.data.iter().enumerate() {
            write!(self.w, "data.{}: {}\n", i, data)?;
        }

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &parser::Stmt) -> std::io::Result<()> {
        match stmt {
            parser::Stmt::Expr(expr) => match expr {
                parser::Expr::Literal(_) => {}
                parser::Expr::Ident(_) => {}
                parser::Expr::FnCall(fn_call) => {
                    self.generate_fn_call(fn_call)?;
                }
            },
            parser::Stmt::Assign(assign) => self.generate_assign(assign)?,
        }
        Ok(())
    }

    fn generate_fn_call(&mut self, fn_call: &parser::FnCall) -> std::io::Result<()> {
        for (arg, reg) in Iterator::zip(fn_call.args.iter(), REGISTERS.iter()) {
            self.move_to_reg(reg, arg)?;
        }
        let ident = self
            .scoped_ident(*fn_call.ident, self.current_env)
            .unwrap()
            // TODO: Make it more efficient
            .into_owned();
        write!(self.w, "  call {}\n", ident)
    }

    fn generate_assign(&mut self, assign: &parser::Assign) -> std::io::Result<()> {
        let type_ = self.expr_type(&assign.expr).clone();

        self.move_to_reg("rax", &assign.expr)?;

        self.generate_value_assign(assign.ident.to_string(), type_)
    }

    fn generate_value_assign(&mut self, ident: String, type_: AppliedType) -> std::io::Result<()> {
        let cell_count = type_.cell_count();
        write!(self.w, "  sub rsp, {}\n", cell_count * 8)?;
        let offset = self
            .get_variables(self.current_env)
            .map(|v| v.1.offset)
            .max()
            .unwrap_or_default()
            + cell_count;
        write!(
            self.w,
            "  mov rdi, rsp\n  mov rsi, rax\n  mov rcx, {}\n  rep movsq\n",
            cell_count,
        )?;
        self.insert_variable(ident, Variable { type_, offset }, self.current_env);

        Ok(())
    }

    fn move_to_reg(&mut self, reg: &str, expr: &parser::Expr) -> std::io::Result<()> {
        match expr {
            parser::Expr::Literal(literal) => match literal {
                parser::Literal::Number(n) => {
                    write!(self.w, "  mov {}, {}\n", reg, n)?;
                }
                parser::Literal::String(str) => {
                    self.create_string(str)?;
                    write!(self.w, "  mov {}, rsp\n", reg,)?;
                }
            },
            parser::Expr::Ident(ident) => {
                let offset = {
                    let variable = self.get_variable(**ident, self.current_env).unwrap();
                    variable.offset
                };
                write!(self.w, "  lea {}, [rbp-{}]\n", reg, offset * 8)?;
            }
            parser::Expr::FnCall(fn_call) => {
                self.generate_fn_call(fn_call)?;
                write!(self.w, "  mov {}, rax\n", reg)?;
            }
        }
        Ok(())
    }
}

pub fn compile(program: parser::Program, args: &Args) -> std::io::Result<()> {
    if args.print_asm {
        compile_to_stdout(program, args)
    } else {
        compile_to_file(program, args)
    }
}

fn compile_to_stdout(program: parser::Program, args: &Args) -> std::io::Result<()> {
    let mut compiler = Compiler::new(std::io::stdout());
    compiler.compile(program, &args.path)
}

fn compile_to_file(program: parser::Program, args: &Args) -> std::io::Result<()> {
    let output = args.output.as_ref().map(Cow::from).unwrap_or_else(|| {
        let stem = args.path.file_stem().unwrap();
        let file = Path::new(stem);
        file.into()
    });
    let fasm_path = output.as_ref().with_extension("fasm");
    let fasm = File::create(&fasm_path)?;
    let mut compiler = Compiler::new(fasm);
    compiler.compile(program, &args.path)?;
    Command::new("fasm").arg(&fasm_path).status()?;
    std::fs::remove_file(&fasm_path)?;
    Ok(())
}
