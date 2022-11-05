(mygtd-db-query [:select * :from project])

(mygtd-db-query [:select * :from task])
(mygtd-db-query [:select * :from migrate])
(mygtd-db-query [:select * :from order])

(mygtd-db-order-idlst "20221025")
(mygtd-db-migrate-tasks "20221025")
(mygtd-db-query [:select * :from order :where (= time "20221025")])
(mygtd-db-order-records "20221025")

(mygtd-db-query [:delete :from order :where (= time "20221025")])

(mygtd-db-query [:select * :from task :where (= id "d7f657a1-b853-4c02-a49a-8ada27dcc806")])
(mygtd-db-query [:select * :from migrate :where (= id "d7f657a1-b853-4c02-a49a-8ada27dcc806")])
(mygtd-db-query [:delete :from task :where (= id "d7f657a1-b853-4c02-a49a-8ada27dcc806")])

(mygtd-query-wrapper 'task (car (mygtd-db-query [:select * :from task])))

(mygtd-db-query [:select * :from task :where (= id "c780d1d1-6b1b-436f-bb7a-c4ebf08d63e2")])

(mygtd-db-query [:select * :from migrate :where (= id "c780d1d1-6b1b-436f-bb7a-c4ebf08d63e2")])

(mygtd-db-migrate-timelst "c780d1d1-6b1b-436f-bb7a-c4ebf08d63e2")

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

(mygtd-task-multi-add
 '((:name "测试任务111" :category "test"
          :time "20221105" :details "这是测试任务111的具体内容"
          :period "morning" :deadline "20221104" :location "office" :device "PC")
   (:name "测试任务222" :category "test"
          :time "20221105" :details "这是测试任务222的具体内容
1.这是第一条任务明细
2.这是第二条任务明细
3.这是第三条任务明细"
          :period "morning" :deadline "20221105" :location "office" :device "PC")
   (:name "测试任务333" :category "test"
          :time "20221105" :details "这是测试任务333的具体内容"
          :period "afternoon" :deadline "20221104" :location "home" :device "Mac")
   (:name "测试任务444" :category "test"
          :time "20221105" :details "这是测试任务444的具体内容，这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容...这是一条很长的具体内容..."
          :period "afternoon" :deadline "20221105" :location "outside" :device "Kindle")
   (:name "测试任务555" :category "test"
          :time "20221105" :details "这是测试任务555的具体内容"
          :period "evening" :deadline "20221104" :location "office" :device "iPhone")))G..

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
