(mygtd-db-query [:select * :from task])
(mygtd-db-query [:select * :from migrate])
(mygtd-db-query [:select * :from order])
(mygtd-db-query [:select * :from project])

(mygtd-db-query [:delete :from task])
(mygtd-db-query [:delete :from migrate])

(mygtd-task-multi-add
 '((:name "错单回收测试(文档)" :category "work" :time "20221021")
   (:name "错单分发测试(文档)" :category "work" :time "20221021")
   (:name "错单对外接口测试(文档)" :category "work" :time "20221021")
   (:name "测试用例:BSCHUN_FUNC_20220922_0010" :category "work" :time "20221020")
   (:name "reflect, reflect, reflect" :category "life" :time "20221020")
   (:name "周报编写(在线文档、信点兵)" :category "work" :time "20221020")
   (:name "svn,ppt进度更新" :category "work" :time "20221020")
   (:name "错单回收测试" :category "work"  :time "20221020")
   (:name "健身房跑步半小时" :category "life" :time "20221020")
   (:name "晚上10点半按时睡觉" :category "life" :time "20221020")
   (:name "错单回收时序问题" :category "work" :time "20221018")
   (:name "支持多个分析标志位查询" :category "work" :time "20221018")
   (:name "错单返回的标题按照最终展示的顺序返给前台" :category "work" :time "20221018")
   (:name "mygtd构建一个展示任务列表的可用版本" :category "hack" :time "20221022")
   (:name "200031错单时序问题回收成功场景测试" :category "work" :time "20221022")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mygtd-db-query [:insert :into order :values (["20220926" "111,222,333,444"])])
(mygtd-db-query [:delete :from order])

;; 页面任务迁移保存的时候，如果迁移到当前的time，不产生迁移记录

;; (mygtd-db-query [:select * :from migrate])
;; (mygtd-db-query [:delete :from migrate])

;; (mygtd-db-query `[:insert-into migrate :values (["111" "20221014"])])
(mygtd-db-query `[:insert-into migrate :values (["111" "20221015"])])
(mygtd-db-query `[:insert-into migrate :values (["111" "202210"])])
(mygtd-db-query `[:insert-into migrate :values (["111" "20221016"])])

;; (mygtd-db-migrate-records "111")
;; (mygtd-db-migrate-tasks "20220926")

;; (mygtd-migrated-icon "202209" "202210")

(mygtd-db-migrate-timelst "222")
(mygtd-task-icon "222" "20220926")
