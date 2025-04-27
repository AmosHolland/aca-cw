use crate::memory;

#[derive(Clone, Copy, Debug)]
pub enum RuntimeCommand {
    Step,
    ShowRegisters,
    ShowRegister(usize),
    ShowMemory((usize, usize)),
    ShowPipeline,
    ShowPC,
    ShowBranchPredictor,
    ShowReturnCache,
    Nop,
}

pub enum PreRuntimeCommand {
    Run(String),
    Debug(String),
    Test(usize),
}

pub fn parse_runtime_command_string(line: String) -> Result<RuntimeCommand, String> {
    let parts: Vec<&str> = line.split(' ').collect();
    let op = parts[0];
    let n_parts = parts.len();

    match (op, n_parts) {
        ("s", 1) => Ok(RuntimeCommand::Step),
        ("r", 1) => Ok(RuntimeCommand::ShowRegisters),
        ("r", 2) => {
            let Ok(n) = parts[1].parse() else {
                return Err(format!("Could not parse {0} into a number.", parts[1]));
            };
            if n >= crate::ARF_SIZE {
                return Err(format!("Register number {n} is too large."));
            }

            Ok(RuntimeCommand::ShowRegister(n))
        }
        ("m", 3) => {
            let (Ok(n1), Ok(n2)) = (parts[1].parse(), parts[2].parse()) else {
                return Err(format!(
                    "Could not parse both {0} and {1} into numbers.",
                    parts[1], parts[2]
                ));
            };

            if n1 >= memory::MEM_SIZE || n2 >= memory::MEM_SIZE || n1 > n2 {
                return Err(format!("Invalid start and end addresses {n1} and {n2}."));
            }

            Ok(RuntimeCommand::ShowMemory((n1, n2)))
        }
        ("pl", 1) => Ok(RuntimeCommand::ShowPipeline),
        ("pc", 1) => Ok(RuntimeCommand::ShowPC),
        ("b", 1) => Ok(RuntimeCommand::ShowBranchPredictor),
        ("j", 1) => Ok(RuntimeCommand::ShowReturnCache),
        ("", 1) => Ok(RuntimeCommand::Nop),
        _ => Err("Invalid command.".to_string()),
    }
}

pub fn parse_pre_runtime_command_string(line: String) -> Result<PreRuntimeCommand, String> {
    let parts: Vec<&str> = line.split(' ').collect();
    let op = parts[0];
    let n_parts = parts.len();

    match (op, n_parts) {
        ("run", 2) => Ok(PreRuntimeCommand::Run(parts[1].to_string())),
        ("debug", 2) => Ok(PreRuntimeCommand::Debug(parts[1].to_string())),
        ("test", 2) => match parts[1].to_string().parse::<usize>() {
            Ok(n) => Ok(PreRuntimeCommand::Test(n)),
            Err(e) => Err(e.to_string()),
        },
        _ => Err("Invalid Command".to_string()),
    }
}
