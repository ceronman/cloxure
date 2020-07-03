(ns cloxure.ast)

(defn binary [left operator right]
  {::type ::binary ::left left ::right right ::operator operator})

(defn call [callee paren arguments]
  {::type ::call ::callee callee ::paren paren ::arguments arguments})

(defn get-expr [object name-token]
  {::type ::get-expr ::object object ::name-token name-token})

(defn set-expr [object name-token value]
  {::type ::set-expr ::object object ::name-token name-token ::value value})

(defn unary [operator right]
  {::type ::unary ::operator operator ::right right})

(defn literal [value]
  {::type ::literal ::value value})

(defn logical [left operator right]
  {::type ::logical ::left left ::right right ::operator operator})

(defn variable [name-token]
  {::type ::variable ::name-token name-token})

(defn group [expression]
  {::type ::group ::expression expression})

(defn print-stmt [expression]
  {::type ::print-stmt ::expression expression})

(defn return-stmt [keyword value]
  {::type ::return-stmt ::keyword keyword ::value value})

(defn var-stmt [name-token initializer]
  {::type ::var-stmt ::name-token name-token ::initializer initializer})

;; TODO: value-expr not consistent with value in other parts
(defn assign [name-token value-expr]
  {::type ::assign ::name-token name-token ::value-expr value-expr})

;; TODO: statements not consistent with body in other parts
(defn block [statements]
  {::type ::block ::statements statements})

(defn if-stmt [condition, then-branch, else-branch]
  {::type ::if-stmt ::condition condition ::then-branch then-branch ::else-branch else-branch})

(defn while-stmt [condition body]
  {::type ::while-stmt ::condition condition ::body body})

(defn fun-stmt [name-token params body]
  {::type ::fun-stmt ::name-token name-token ::params params ::body body})

(defn class-stmt [name-token superclass methods]
  {::type ::class-stmt ::name-token name-token ::superclass superclass ::methods methods})

(defn this-expr [keyword]
  {::type ::this-expr ::keyword keyword})

(defn super [keyword method]
  {::type ::super ::keyword keyword ::method method})