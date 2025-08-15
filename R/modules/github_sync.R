library(usethis)

git_sitrep()

gert::git_add(".")
gert::git_commit_all()
# gert::git_commit("Din commit besked")
gert::git_push()