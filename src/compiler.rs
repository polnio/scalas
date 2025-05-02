use crate::args::Args;
use crate::parser;
use convert_case::{Case, Casing as _};
use derive_more::Display;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

const REGISTERS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

#[derive(Debug, Clone, PartialEq, Eq, Display)]
enum PtrValue {
    Label(String),
    // Address(usize),
}
#[derive(Debug, Clone, PartialEq, Eq, Display)]
enum PrimitiveValue {
    Int(i64),
    QWord(usize),
    Ptr(PtrValue),
    Reg(String),
}
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct StructValue {
    data: IndexMap<String, PrimitiveValue>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Primitive(PrimitiveValue),
    Struct(StructValue),
}
impl Value {
    fn size(&self) -> usize {
        match self {
            Value::Primitive(_) => 8,
            // Value::Struct(st) => st.data.values().map(Value::size).sum::<usize>(),
            Value::Struct(st) => st.data.values().len() * 8,
        }
    }
}

struct Compiler<W: Write> {
    w: W,
    variables: IndexMap<String, (usize, Value)>,
    // types: IndexMap<String, Value>,
    data: Vec<String>,
}
impl<W: Write> Compiler<W> {
    pub fn new(w: W) -> Self {
        Self {
            w,
            variables: Default::default(),
            // types: Default::default(),
            data: Default::default(),
        }
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
            self.variables.clear();
            for fn_ in object.fns {
                write!(
                    self.w,
                    "{}.{}:\n  push rbp\n  mov rbp, rsp\n",
                    *object.ident, *fn_.ident
                )?;
                for stmt in &fn_.body {
                    self.compile_stmt(stmt)?;
                }
                write!(
                    self.w,
                    "  add rsp, {}\n  pop rbp\n  ret\n",
                    self.variables.values().map(|v| v.1.size()).sum::<usize>()
                )?;
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

    fn create_string(&mut self, s: &str) -> (String, Value) {
        let n = self.data.len();
        let name = format!("data.{}", n);
        self.data.push(format!("db \"{}\"", s));
        let value = Value::Struct(StructValue {
            data: IndexMap::from([
                (
                    "data".to_string(),
                    PrimitiveValue::Ptr(PtrValue::Label(name.clone())),
                ),
                ("len".to_string(), PrimitiveValue::QWord(s.len())),
            ]),
        });
        (name, value)
    }

    fn generate_assign(&mut self, assign: &parser::Assign) -> std::io::Result<()> {
        let value: Value = match &assign.expr {
            parser::Expr::Literal(literal) => match literal {
                parser::Literal::Number(n) => Value::Primitive(PrimitiveValue::Int(*n)),
                parser::Literal::String(s) => self.create_string(s).1,
            },
            parser::Expr::Ident(ident) => {
                let value = self.variables.get(**ident).unwrap();
                value.1.clone()
            }
            parser::Expr::FnCall(fn_call) => {
                self.generate_fn_call(fn_call)?;
                Value::Primitive(PrimitiveValue::Reg("rax".to_owned()))
            }
        };

        self.generate_value_assign(assign.ident.to_string(), value)
    }

    fn generate_value_assign(&mut self, ident: String, value: Value) -> std::io::Result<()> {
        write!(self.w, "  sub rsp, {}\n", value.size())?;
        let offset = self.variables.values().map(|v| v.1.size()).sum::<usize>() + value.size();
        let mut coffset = 0;
        match &value {
            Value::Primitive(p) => {
                write!(self.w, "  mov qword [rsp-{}], {}\n", offset - coffset, p)?
            }
            Value::Struct(st) => {
                for v in st.data.values() {
                    write!(
                        self.w,
                        "  mov qword [rbp-{}], {}\n",
                        offset - coffset,
                        v.to_string()
                    )?;
                    coffset += 8
                }
            }
        }
        self.variables.insert(ident, (offset, value));

        Ok(())
    }

    fn move_to_reg(&mut self, reg: &str, expr: &parser::Expr) -> std::io::Result<()> {
        match expr {
            parser::Expr::Literal(literal) => match literal {
                parser::Literal::Number(n) => {
                    write!(self.w, "  mov {}, {}\n", reg, n)?;
                }
                parser::Literal::String(str) => {
                    let offset = self.variables.values().map(|v| v.1.size()).sum::<usize>();
                    let (name, value) = self.create_string(str);
                    self.generate_value_assign(name, value.clone())?;
                    write!(self.w, "  lea {}, [rbp-{}]\n", reg, offset + value.size())?;
                }
            },
            parser::Expr::Ident(ident) => {
                let value = self.variables.get(**ident).unwrap();
                write!(self.w, "  lea {}, [rbp-{}]\n", reg, value.0)?;
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
