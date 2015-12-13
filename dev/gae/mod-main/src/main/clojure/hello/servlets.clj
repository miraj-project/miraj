(ns hello.servlets)

(gen-class :name config
           :extends javax.servlet.http.HttpServlet
           :exposes-methods {init initParent}
           :impl-ns config)

(gen-class :name hello.reloader
           :implements [javax.servlet.Filter]
           :impl-ns hello.reloader)
