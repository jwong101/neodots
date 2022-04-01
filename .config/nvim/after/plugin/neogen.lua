local ok, neogen = pcall(require, "neogen")
if not ok then
  return
end

neogen.setup {
  enabled = true,
  input_after_comment = true,
  languages = {
    lua = {
      template = {
        annotation_convention = "emmylua",
      },
    },
    rust = {
      template = {
        annotation_convention = "rustdoc",
      },
    },
  },
}
