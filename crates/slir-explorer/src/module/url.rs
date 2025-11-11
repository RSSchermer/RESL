use slir::Function;
use urlencoding::encode as urlencode;

pub fn function_url(function: Function) -> String {
    format!(
        "/{}/functions/{}",
        urlencode(function.module.as_str()),
        urlencode(function.name.as_str())
    )
}
