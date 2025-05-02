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

static NUMBER_TYPE: LazyLock<AppliedType> = LazyLock::new(|| AppliedType {
    type_: Type {
        cell_count_fn: Box::new(|_| 1),
    },
    generics: Default::default(),
});
static STRING_TYPE: LazyLock<AppliedType> = LazyLock::new(|| AppliedType {
    type_: Type {
        cell_count_fn: Box::new(|_| 2),
    },
    generics: Default::default(),
});

struct Variable {
    type_: AppliedType,
    offset: usize,
}

#[derive(Clone)]
struct Type {
    cell_count_fn: Box<fn(&[AppliedType]) -> usize>,
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

type EnvKey = slotmap::DefaultKey;

#[derive(Default)]
struct Env {
    parent: Option<EnvKey>,
    variables: IndexMap<String, Variable>,
    types: IndexMap<String, Type>,
    children: Vec<EnvKey>,
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
    fn get_variables(&self, env: EnvKey) -> impl Iterator<Item = (&String, &Variable)> {
        let env = self.envs.get(env).unwrap();
        env.variables.iter()
    }
    fn get_variable(&self, ident: &str, env: EnvKey) -> Option<&Variable> {
        let env = self.envs.get(env).unwrap();
        if let Some(variable) = env.variables.get(ident) {
            return Some(variable);
        }
        if let Some(parent) = env.parent {
            return self.get_variable(ident, parent);
        }
        None
    }
    fn insert_variable(&mut self, ident: String, value: Variable, env: EnvKey) {
        let env = self.envs.get_mut(env).unwrap();
        env.variables.insert(ident, value);
    }
    fn compile(&mut self, program: parser::Program, path: &Path) -> std::io::Result<()> {
        let obj_name = path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_case(Case::Pascal);
        write!(
            self.w,
            "format ELF64 executable\n_start:\n  call {}.main\n  mov rax, 60\n  xor rdi, rdi\n  syscall\n",
            obj_name
        )?;
        for object in program.objects {
            for fn_ in object.fns {
                write!(
                    self.w,
                    "{}.{}:\n  push rbp\n  mov rbp, rsp\n",
                    *object.ident, *fn_.ident
                )?;
                for stmt in &fn_.body {
                    self.compile_stmt(stmt)?;
                }
                write!(self.w, "  mov rsp, rbp\n  pop rbp\n  ret\n")?;
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
        write!(self.w, "  call {}\n", *fn_call.ident)
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
            "  lea rdi, [rbp-{}]\n  mov rsi, rax\n  mov rcx, {}\n  rep movsq\n",
            offset * 8,
            cell_count * 8,
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
