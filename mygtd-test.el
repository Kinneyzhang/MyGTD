(mygtd-db-query [:select * :from task])
(mygtd-db-query [:select * :from migrate])
(mygtd-db-query [:select * :from order])
(mygtd-db-query [:select * :from project])

(mygtd-task-multi-add
 '((:name "费曼学习法第七章总结" :category "work" :time "20220924")
   (:name "webSecurity 验证问题配合测试" :category "study" :time "20220924")
   (:name "根据 so_nbr 和 deal_time 更新错单(刘璇)" :category "work" :time "20220924")
   (:name "修改相关处理逻辑：判断是否需要回收" :category "work" :time "20220924")
   (:name "参数加载配置文件修改(刘璇)" :category "work" :time "20220924")
   (:name "概要设计文档" :category "study" :time "20220924")
   (:name "详细设计文档" :category "work" :time "20220924")
   (:name "analyse表: 增加字段 script_no 外键" :category "work"  :time "20220924")
   (:name "列出 mygtd 需要开发的核心功能列表 后写架构性的方法。" :category "work" :time "20220924")
   (:name "如何玩，如何会玩，如何才能使身心得到真正的方式和休息" :category "work" :time "20220924")
   (:name "阅读总结《费曼学习法》第五六章" :category "work" :time "20220924")
   (:name "研究 hammerspoon 的功能" :category "work" :time "20220924")))


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
