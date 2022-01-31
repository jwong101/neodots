local loaded, lint = pcall(require, 'lint')

if not loaded then
  return
end

lint.linters_by_ft = {
  python = {'flake8'},
  sh = {'shellcheck'},
  bash = {'shellcheck'},
  zsh = {'shellcheck'},
}
