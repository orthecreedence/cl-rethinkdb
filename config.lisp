(in-package :cl-rethinkdb)

(defvar *query-gc-threshold* 30
  "How many seconds to wait AFTER a query has changed state before it is ok to
   remove that query. For instance, a partial result may come back from a query
   but the driver will keep that query 'active' until it is finished. We don't
   want a bunch of unused partial queries laying around, but we also don't want
   to get rid of queries that may be used. This parameter lets the app tune how
   many seconds pass between when a partial result from a query returns and when
   that query is garbage collected. Set to nil to disable.
   
   Also keep in mind that queries that finish immediately or return errors are
   immediately garbage collected.")
