;; TODO: put this to layer.el in the upcoming version
(configuration-layer/declare-layers '(yxl-ui
                                        ; yxl layer:
                                        ; general personal preference settings
                                        ; and modifications to spacemacs layers
                                        ; + bindings related to evilified state
                                      yxl-general
                                      yxl-evil
                                      yxl-prog
                                      yxl-text
                                      yxl-dired
                                      yxl-ess
                                      yxl-org
                                      yxl-web
                                      yxl-helm))

(setq yxl/dotfiles "~/dotfiles/")
(setq yxl/Dropbox "~/Dropbox/")
(setq yxl/Downloads "~/Downloads/")

(setq yxl/file-dot-inbox "~/Downloads/.inbox")
(setq yxl/file-task-today  "~/Dropbox/org/TODAY.org")
(setq yxl/file-task-scratch "~/Dropbox/org/scratch.org")
(setq yxl/file-task-project-collection "~/Dropbox/org/proj_CTW.org")
(setq yxl/task-project-meta "~/Downloads/c3/c3_pwd/tex/meta.org")

;; bread and butter
(setq yxl/code-repo "~/Downloads/c3_pwd/")
(setq yxl/phd-repo "~/Downloads/yxl105_ctw/")
(setq yxl/code-pwd "~/Downloads/c3_pwd/code/")
(setq yxl/paper-pwd "~/Downloads/yxl105_ctw/yxl105_tex/")
(setq yxl/journal-pwd "~/Dropbox/lit_1_yxl105/1_thesis/")
(setq yxl/file-bib
      (concat yxl/paper-pwd "yxl105_bib/yxl105_bib_master.bib"))
