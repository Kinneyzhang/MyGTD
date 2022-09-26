(mygtd-db-query [:select * :from task])
(mygtd-db-query [:select * :from migrate])
(mygtd-db-query [:select * :from order])
(mygtd-db-query [:select * :from project])

(mygtd-db-query [:delete :from task])
(mygtd-db-query [:delete :from migrate])

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

(mygtd-task-multi-add
 '((:id "111" :name "test111" :category "work" :time "20220926")
   (:id "222" :name "test222" :category "study" :time "20220926")
   (:id "333" :name "test333" :category "work" :time "20220926")
   (:id "444" :name "test444" :category "study" :time "20220926")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mygtd-db-query [:insert :into order :values (["20220926" "111,222,333,444"])])
(mygtd-db-query [:delete :from order])
