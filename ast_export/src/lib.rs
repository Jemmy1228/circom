use std::fs::File;
use std::path::PathBuf;
use std::io::Write;
use std::io::BufWriter;

use program_structure::bus_data::BusData;
use program_structure::function_data::FunctionData;
use program_structure::program_archive::ProgramArchive;
use program_structure::template_data::TemplateData;

mod statement;
mod expression;

pub struct ExportConfig {
    pub flag_json_ast: bool,
    pub json_ast_folder: String,
}

pub fn export_ast(program: &ProgramArchive, config: ExportConfig) {
    if !config.flag_json_ast {
        return;
    }

    let dir = PathBuf::from(&config.json_ast_folder);
    std::fs::create_dir_all(&dir).unwrap();

    for (id, data) in &program.templates {
        let path = dir.join(format!("{}.json", id));
        let file = File::create(path).unwrap();
        let mut writer = Box::new(BufWriter::new(file)) as Box<dyn Write>;
        export_template(&mut writer, data);
    }

    for (id, data) in &program.functions {
        let path = dir.join(format!("{}.json", id));
        let file = File::create(path).unwrap();
        let mut writer = Box::new(BufWriter::new(file)) as Box<dyn Write>;
        export_function(&mut writer, data);
    }

    for (id, data) in &program.buses {
        let path = dir.join(format!("bus_{}.json", id));
        let file = File::create(path).unwrap();
        let mut writer = Box::new(BufWriter::new(file)) as Box<dyn Write>;
        export_bus(&mut writer, data);
    }
}

fn export_template(writer: &mut Box<dyn Write>, data: &TemplateData) {
    writeln!(writer, "{{").unwrap();
    writeln!(writer, "\"$\": \"Ast\",").unwrap();
    writeln!(writer, "\"@\": \"TemplateData\",").unwrap();
    writeln!(writer, "\"file_id\": \"{}\",", data.get_file_id()).unwrap();
    writeln!(writer, "\"name\": \"{}\",", data.get_name()).unwrap();
    writeln!(writer, "\"num_of_params\": {},", data.get_num_of_params()).unwrap();
    write!(writer, "\"name_of_params\": [").unwrap();
    for (i, param_name) in data.get_name_of_params().iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        write!(writer, "\"{}\"", param_name).unwrap();
    }
    writeln!(writer, "],").unwrap();
    writeln!(writer, "\"is_parallel\": {},", data.is_parallel()).unwrap();
    writeln!(writer, "\"is_custom_gate\": {},", data.is_custom_gate()).unwrap();
    writeln!(writer, "\"is_extern_c\": {},", data.is_extern_c()).unwrap();
    write!(writer, "\"body\": ").unwrap();
    statement::export_statement(writer, data.get_body());
    writeln!(writer).unwrap();
    writeln!(writer, "}}").unwrap();
}

fn export_function(writer: &mut Box<dyn Write>, data: &FunctionData) {
    writeln!(writer, "{{").unwrap();
    writeln!(writer, "\"$\": \"Ast\",").unwrap();
    writeln!(writer, "\"@\": \"FunctionData\",").unwrap();
    writeln!(writer, "\"name\": \"{}\",", data.get_name()).unwrap();
    writeln!(writer, "\"file_id\": \"{}\",", data.get_file_id()).unwrap();
    writeln!(writer, "\"num_of_params\": {},", data.get_num_of_params()).unwrap();
    write!(writer, "\"name_of_params\": [").unwrap();
    for (i, param_name) in data.get_name_of_params().iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        write!(writer, "\"{}\"", param_name).unwrap();
    }
    writeln!(writer, "],").unwrap();
    write!(writer, "\"body\": ").unwrap();
    statement::export_statement(writer, data.get_body());
    writeln!(writer).unwrap();
    writeln!(writer, "}}").unwrap();
}

fn export_bus(writer: &mut Box<dyn Write>, data: &BusData) {
    writeln!(writer, "{{").unwrap();
    writeln!(writer, "\"$\": \"Ast\",").unwrap();
    writeln!(writer, "\"@\": \"BusData\",").unwrap();
    writeln!(writer, "\"file_id\": \"{}\",", data.get_file_id()).unwrap();
    writeln!(writer, "\"name\": \"{}\",", data.get_name()).unwrap();
    writeln!(writer, "\"num_of_params\": {},", data.get_num_of_params()).unwrap();
    write!(writer, "\"name_of_params\": [").unwrap();
    for (i, param_name) in data.get_name_of_params().iter().enumerate() {
        if i > 0 {
            write!(writer, ", ").unwrap();
        }
        write!(writer, "\"{}\"", param_name).unwrap();
    }
    writeln!(writer, "]").unwrap();
    writeln!(writer, "}}").unwrap();
}

#[cfg(test)]
mod tests {}
