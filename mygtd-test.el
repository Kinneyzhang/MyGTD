;; (mygtd-db-query
;;  [:select * :from task])

(mygtd-task-multi-add
 '((:name "费曼学习法第七章总结" :category "work" :timestr "20220914,20220915,")
   (:name "webSecurity 验证问题配合测试" :category "study" :timestr "20220915,20220916,")
   (:name "根据 so_nbr 和 deal_time 更新错单(刘璇)" :category "work" :timestr "20220914,")
   (:name "修改相关处理逻辑：根据是否配置了 script_no 判断是否需要回收" :category "work" :timestr "202209,20220915,")
   (:name "参数加载配置文件修改(刘璇)" :category "work" :timestr "20220916,")
   (:name "概要设计文档（需求描述、业务流程、业务流程说明、系统边界）" :category "study" :timestr "20220915,202209,")
   (:name "详细设计文档（系统架构说明、表、接口、demo、伪代码等）" :category "work" :timestr "20220915,")
   (:name "analyse表: 增加字段 script_no 外键" :category "work" :timestr "20220915,202209,202210,")
   (:name "列出 mygtd 需要开发的核心功能列表 后写架构性的方法。" :category "work" :timestr "2022,")
   (:name "如何玩，如何会玩，如何才能使身心得到真正的方式和休息" :category "work" :timestr "2022,2023,")
   (:name "阅读总结《费曼学习法》第五六章" :category "work" :timestr "202209,2022,")
   (:name "研究 hammerspoon 的功能" :category "work" :timestr "202209,2022,202210,")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar example-editors '("emacs" "vim" "vscode" "sublime text"))
(defvar example-websites
  '(("emacs" "https://www.gnu.org/software/emacs/")
    ("vim" "https://www.vim.org")
    ("vscode" "https://code.visualstudio.com")
    ("sublime text" "https://www.sublimetext.com")))

(with-twidget-buffer "*Twidget Test*"
  (twidget-create 'twidget-choice
    :bind 'example-editor
    :choices example-editors
    :format "\nEditors: [t]"
    :value "emacs"
    :separator "/"
    :action (lambda (value)
              (twidget-multi-update
               'example-string `(:value ,(capitalize value))
               'example-link `(:value ,(assoc value example-websites))))
    :require t)
  (twidget-create 'twidget-button
    :value "#switch#"
    :action (lambda (btn)
              (let* ((choices example-editors)
                     (editor (downcase example-editor))
                     (nth (seq-position choices editor)))
                (twidget-update
                 'example-editor
                 :value (nth (% (1+ nth) (length choices)) choices)))))
  (twidget-insert "\n\n")
  (twidget-create 'twidget-text
    :bind 'example-string
    :format "  - [t] is my favorite editor."
    :value "Emacs"
    :plain t)
  (twidget-create 'twidget-text
    :bind 'example-link
    :format "\n  - The website of [t0] is [t1]."
    :value '("emacs" "https://www.gnu.org/software/emacs/")
    :plain t))
