use crate::args::Args;
use crate::error::{AbortStd as _, Error};
use crate::parser;
use crate::tokenizer;
use convert_case::{Case, Casing as _};
use indexmap::IndexMap;
use itertools::Itertools as _;
use slotmap::SlotMap;
use std::borrow::Cow;
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::sync::LazyLock;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Unit;
impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

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

macro_rules! compiler_write {
    ($expr:expr, $($arg:tt)*) => {
        if let Err(e) = write!($expr, $($arg)*) {
            eprintln!("Error while writing to compiler output: {}", e);
            std::process::exit(1);
        }
    };
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
    fn convert_type<'this, 'src>(
        &'this self,
        ty: &parser::Type,
    ) -> Result<AppliedType, Vec<Error<'src, Unit>>> {
        let (generics, errors): (Vec<_>, Vec<_>) = ty
            .generics
            .iter()
            .map(|type_| self.convert_type(type_))
            .partition_result();
        let mut errors = errors.into_iter().flatten().collect::<Vec<_>>();
        let type_ = match self.get_type(&ty.ident, self.current_env).cloned() {
            Some(type_) => type_,
            None => {
                errors.push(Error::custom(
                    ty.ident.span,
                    format!("Unknown type {}", ty.ident),
                ));
                return Err(errors);
            }
        };
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(AppliedType { type_, generics })
    }
    fn expr_type<'src>(
        &mut self,
        expr: &parser::Expr,
    ) -> Result<&AppliedType, Vec<Error<'src, Unit>>> {
        match expr {
            parser::Expr::Literal(l) => match &**l {
                tokenizer::Literal::Number(_) => Ok(&NUMBER_TYPE),
                tokenizer::Literal::String(_) => Ok(&STRING_TYPE),
            },
            parser::Expr::FnCall(fn_call) => {
                let fn_ = self.expr_type(&parser::Expr::Ident(fn_call.ident.clone()))?;
                // Expect Fn[Args, Ret]
                let type_ = fn_.generics.first().unwrap();
                Ok(type_)
            }
            parser::Expr::Ident(ident) => {
                let variable = self.get_variable(ident, self.current_env).ok_or_else(|| {
                    vec![Error::custom(
                        ident.span,
                        format!("Unknown variable {}", ident),
                    )]
                })?;

                Ok(&variable.type_)
            }
            parser::Expr::Block(block) => {
                self.compile_block(block)?;
                Ok(&UNIT_TYPE)
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
    fn scoped<'this, 'ident>(&'this self, ident: &'ident str, env: EnvKey) -> Cow<'ident, str> {
        match self.scope(env) {
            Some(scope) => format!("{}.{}", scope, ident).into(),
            None => ident.into(),
        }
    }
    fn scoped_ident<'this, 'ident>(
        &'this self,
        ident: &'ident str,
        env: EnvKey,
    ) -> Option<Cow<'ident, str>> {
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
    fn create_string<'this, 'src>(&'this mut self, s: &str) -> Result<(), Vec<Error<'src, Unit>>> {
        let n = self.data.len();
        let name = format!("data.{}", n);
        compiler_write!(
            self.w,
            "  sub rsp, 16\n  mov qword [rsp], {}\n  mov qword [rsp+8], {}\n",
            name,
            s.len()
        );
        self.data.push(format!("{} db \"{}\"", name, s));
        Ok(())
    }
    impl_get_set!(variable: Variable);
    impl_get_set!(function: Function);
    impl_get_set!(type: Type);
    fn compile<'this, 'src>(
        &'this mut self,
        program: parser::Program,
        path: &Path,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        let obj_name = path
            .file_stem()
            // Already checked since it's a file
            .unwrap()
            .to_str()
            .unwrap()
            .to_case(Case::Pascal);
        compiler_write!(
            self.w,
            "format ELF64 executable\n_start:\n{}  xor r8, r8\n.loop:\n  cmp r8, [rbp+8]\n  je .done\n  inc r8\n  sub rsp, 16\n  mov rdi, [rbp+r8*8+8]\n  mov qword [rsp], rdi\n  call internals.strlen\n  mov qword [rsp+8], rax\n  jmp .loop\n.done:\n  sub rsp, 16\n  mov rax, [rsp+16]\n  mov [rsp], rax\n  mov [rsp+8], r8\n  mov rdi, rsp\n  call {}.main\n  mov rsp, rbp\n  pop rbp\n  mov rax, 60\n  xor rdi, rdi\n  syscall\n",
            FN_PRELUDE,
            obj_name
        );

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

        let mut errors = Vec::new();
        let objects_envs = program
            .objects
            .iter()
            .map(|object| {
                let object_env = self.subenv(object.ident.to_string(), self.current_env);
                self.insert_function(
                    object.ident.to_string(),
                    Function {
                        args: Default::default(),
                        ret: UNIT_TYPE.clone(),
                    },
                    self.current_env,
                );
                object
                    .fns
                    .iter()
                    .filter_map(|fn_| {
                        if self.get_function(&fn_.ident, object_env).is_some() {
                            errors.extend(vec![Error::custom(
                                fn_.ident.span,
                                format!("Function {} is already defined", fn_.ident),
                            )]);
                            return None;
                        }
                        let (args, errs): (Vec<_>, Vec<_>) = fn_
                            .args
                            .iter()
                            .map(|(_ident, type_)| self.convert_type(type_))
                            .partition_result();
                        errors.extend(errs.into_iter().flatten());
                        let ret = match self.convert_type(&fn_.ret) {
                            Ok(ret) => ret,
                            Err(errs) => {
                                errors.extend(errs);
                                return None;
                            }
                        };
                        self.insert_function(
                            fn_.ident.to_string(),
                            Function { args, ret },
                            object_env,
                        );
                        let fn_env = self.subenv(fn_.ident.to_string(), object_env);
                        Some((object_env, fn_env))
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        if !errors.is_empty() {
            return Err(errors);
        }

        const FN_PRELUDE: &str = "  push rbp\n  mov rbp, rsp\n";
        const FN_EPILOG: &str = "  mov rsp, rbp\n  pop rbp\n  ret\n";

        for (object, envs) in program.objects.iter().zip(objects_envs) {
            compiler_write!(
                self.w,
                "{}:\n  mov al, [data.{}.init]\n  test al, al\n  jz $ + 3\n  ret\n{}  mov byte [data.{}.init], 1\n",
                *object.ident,
                *object.ident,
                FN_PRELUDE,
                *object.ident
            );
            self.data.push(format!("data.{}.init db 0", *object.ident));
            self.compile_block(&object.ctor)?;
            compiler_write!(self.w, "{}", FN_EPILOG);
            for (fn_, (_object_env, fn_env)) in object.fns.iter().zip(envs) {
                compiler_write!(self.w, "{}.{}:\n{}", *object.ident, *fn_.ident, FN_PRELUDE);
                let parent_env = self.current_env;
                self.current_env = fn_env;
                for (i, (ident, type_)) in fn_.args.iter().enumerate() {
                    compiler_write!(self.w, "  mov rax, {}\n", REGISTERS[i]);
                    let type_ = match self.convert_type(type_) {
                        Ok(type_) => type_,
                        Err(errs) => {
                            errors.extend(errs);
                            continue;
                        }
                    };
                    self.generate_value_assign(ident.to_string(), type_)?;
                }
                compiler_write!(self.w, "  call {}\n", *object.ident);
                self.move_to_reg("rax", &fn_.body)?;
                compiler_write!(self.w, "{}", FN_EPILOG);
                self.current_env = parent_env;
            }
        }

        compiler_write!(self.w, "exit:\n  mov rax, 60\n  syscall\n  ret\n");
        compiler_write!(
            self.w,
            "print:\n  mov rsi, [rdi]\n  mov rdx, [rdi+8]\n  mov rax, 1\n  mov rdi, 1\n  syscall\n  ret\n"
        );
        compiler_write!(
            self.w,
            "println:\n  call print\n  mov rax, 1\n  mov rdi, 1\n  mov rsi, data.newline\n  mov rdx, 1\n  syscall\n  ret\n"
        );
        compiler_write!(
            self.w,
            "internals.strlen:\n  xor rax,rax\n.loop:\n  cmp byte [rdi], 0\n  je .done\n  inc rax\n  inc rdi\n  jmp .loop\n.done:\n  ret\n"
        );

        compiler_write!(self.w, "data.newline db 10\n");
        for data in self.data.iter() {
            compiler_write!(self.w, "{}\n", data);
        }

        Ok(())
    }

    fn compile_block<'this, 'src>(
        &'this mut self,
        block: &parser::Block,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        let mut errors = Vec::new();
        for stmt in &block.stmts {
            if let Err(errs) = self.compile_stmt(stmt) {
                errors.extend(errs);
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }

    fn compile_stmt<'this, 'src>(
        &'this mut self,
        stmt: &parser::Stmt,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        match stmt {
            parser::Stmt::Expr(expr) => match expr {
                parser::Expr::Literal(_) => {}
                parser::Expr::Ident(_) => {}
                parser::Expr::FnCall(fn_call) => {
                    self.generate_fn_call(fn_call)?;
                }
                parser::Expr::Block(block) => self.compile_block(block)?,
            },
            parser::Stmt::Assign(assign) => self.generate_assign(assign)?,
        }
        Ok(())
    }

    fn generate_fn_call<'this, 'src>(
        &'this mut self,
        fn_call: &parser::FnCall,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        for (arg, reg) in Iterator::zip(fn_call.args.iter(), REGISTERS.iter()) {
            self.move_to_reg(reg, arg)?;
        }
        let ident = self
            .scoped_ident(*fn_call.ident, self.current_env)
            .ok_or_else(|| {
                vec![Error::custom(
                    fn_call.ident.span,
                    format!("Unknown function {}", fn_call.ident),
                )]
            })?;
        compiler_write!(self.w, "  call {}\n", ident);

        Ok(())
    }

    fn generate_assign<'this, 'src>(
        &'this mut self,
        assign: &parser::Assign,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        let type_ = self.expr_type(&assign.expr).cloned()?;

        self.move_to_reg("rax", &assign.expr)?;

        self.generate_value_assign(assign.ident.to_string(), type_)
    }

    fn generate_value_assign<'this, 'src>(
        &'this mut self,
        ident: String,
        type_: AppliedType,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        let cell_count = type_.cell_count();
        compiler_write!(self.w, "  sub rsp, {}\n", cell_count * 8);
        let offset = self
            .get_variables(self.current_env)
            .map(|v| v.1.offset)
            .max()
            .unwrap_or_default()
            + cell_count;
        compiler_write!(
            self.w,
            "  mov rdi, rsp\n  mov rsi, rax\n  mov rcx, {}\n  rep movsq\n",
            cell_count,
        );
        self.insert_variable(ident, Variable { type_, offset }, self.current_env);

        Ok(())
    }

    fn move_to_reg<'this, 'src>(
        &'this mut self,
        reg: &str,
        expr: &parser::Expr,
    ) -> Result<(), Vec<Error<'src, Unit>>> {
        match expr {
            parser::Expr::Literal(literal) => match &**literal {
                tokenizer::Literal::Number(n) => {
                    compiler_write!(self.w, "  mov {}, {}\n", reg, n);
                }
                tokenizer::Literal::String(str) => {
                    self.create_string(str)?;
                    compiler_write!(self.w, "  mov {}, rsp\n", reg,);
                }
            },
            parser::Expr::Ident(ident) => {
                let offset = {
                    let variable =
                        self.get_variable(**ident, self.current_env)
                            .ok_or_else(|| {
                                vec![Error::custom(
                                    ident.span,
                                    format!("Unknown variable {}", ident),
                                )]
                            })?;
                    variable.offset
                };
                compiler_write!(self.w, "  lea {}, [rbp-{}]\n", reg, offset * 8);
            }
            parser::Expr::FnCall(fn_call) => {
                self.generate_fn_call(fn_call)?;
                compiler_write!(self.w, "  mov {}, rax\n", reg);
            }
            parser::Expr::Block(block) => {
                self.compile_block(block)?;
            }
        }
        Ok(())
    }
}

pub fn compile<'src>(
    program: parser::Program<'src>,
    args: &'src Args,
) -> Result<(), Vec<Error<'src, Unit>>> {
    if args.print_asm {
        compile_to_stdout(program, args)
    } else {
        compile_to_file(program, args)
    }
}

fn compile_to_stdout<'src>(
    program: parser::Program<'src>,
    args: &'src Args,
) -> Result<(), Vec<Error<'src, Unit>>> {
    let mut compiler = Compiler::new(std::io::stdout());
    compiler.compile(program, &args.path)
}

fn compile_to_file<'src>(
    program: parser::Program<'src>,
    args: &'src Args,
) -> Result<(), Vec<Error<'src, Unit>>> {
    let output = args.output.as_ref().map(Cow::from).unwrap_or_else(|| {
        // Already checked since it's a file
        let stem = args.path.file_stem().unwrap();
        let file = Path::new(stem);
        file.into()
    });
    let fasm_path = output.as_ref().with_extension("fasm");
    let fasm = File::create(&fasm_path).unwrap_with_context("Failed to creat asm file");
    let mut compiler = Compiler::new(fasm);
    compiler.compile(program, &args.path)?;
    Command::new("fasm")
        .arg(&fasm_path)
        .status()
        .unwrap_with_context("Failed to run fasm assembler");
    std::fs::remove_file(&fasm_path).unwrap_with_context("Failed to remove asm file");
    Ok(())
}
