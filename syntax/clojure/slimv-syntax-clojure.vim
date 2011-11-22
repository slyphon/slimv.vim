" slimv-syntax-clojure.vim:
"               Clojure syntax plugin for Slimv
" Version:      0.8.6
" Last Change:  09 Aug 2011
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"


"  Load Once:
if exists("b:current_syntax")
  finish
endif

" XXX: this is a real hack-job to get vimclojure highlighting 
" I don't like rainbow parens (they're hard to see w/ desert256) so
" I haven't included them
"
echomsg "Sourcing slimv-syntax-clojure.vim"

if slimv#UseVimClojureHighlighting != 0

    echomsg "Sourcing vimclojure syntax stuff"

    function! s:ColorNamespace(highlights)
        for [category, words] in items(a:highlights)
            if words != []
                execute "syntax keyword clojure" . category . " " . join(words, " ")
            endif
        endfor
    endfunction


    let s:builtins_map = {
        \ "Constant":  "nil",
        \ "Boolean":   "true false",
        \ "Cond":      "if if-not if-let when when-not when-let "
        \            . "when-first cond condp case",
        \ "Exception": "try catch finally throw",
        \ "Repeat":    "recur map mapcat reduce filter for doseq dorun "
        \            . "doall dotimes map-indexed keep keep-indexed",
        \ "Special":   ". def do fn if let new quote var loop",
        \ "Variable":  "*warn-on-reflection* this *assert* "
        \            . "*agent* *ns* *in* *out* *err* *command-line-args* "
        \            . "*print-meta* *print-readably* *print-length* "
        \            . "*allow-unresolved-args* *compile-files* "
        \            . "*compile-path* *file* *flush-on-newline* "
        \            . "*math-context* *unchecked-math* *print-dup* "
        \            . "*print-level* *use-context-classloader* "
        \            . "*source-path* *clojure-version* *read-eval* "
        \            . "*fn-loader* *1 *2 *3 *e",
        \ "Define":    "def- defn defn- defmacro defmulti defmethod "
        \            . "defstruct defonce declare definline definterface "
        \            . "defprotocol defrecord deftype",
        \ "Macro":     "and or -> assert with-out-str with-in-str with-open "
        \            . "locking destructure ns dosync binding delay "
        \            . "lazy-cons lazy-cat time assert with-precision "
        \            . "with-local-vars .. doto memfn proxy amap areduce "
        \            . "refer-clojure future lazy-seq letfn "
        \            . "with-loading-context bound-fn extend extend-protocol "
        \            . "extend-type reify with-bindings ->>",
        \ "Func":      "= not= not nil? false? true? complement identical? "
        \            . "string? symbol? map? seq? vector? keyword? var? "
        \            . "special-symbol? apply partial comp constantly "
        \            . "identity comparator fn? re-matcher re-find re-matches "
        \            . "re-groups re-seq re-pattern str pr prn print "
        \            . "println pr-str prn-str print-str println-str newline "
        \            . "macroexpand macroexpand-1 monitor-enter monitor-exit "
        \            . "eval find-doc file-seq flush hash load load-file "
        \            . "read read-line scan slurp subs sync test "
        \            . "format printf loaded-libs use require load-reader "
        \            . "load-string + +' -' *' /' < <= == >= > dec dec' "
        \            . "inc inc' min max "
        \            . "neg? pos? quot rem zero? rand rand-int decimal? even? "
        \            . "odd? float? integer? number? ratio? rational? "
        \            . "bit-and bit-or bit-xor bit-not bit-shift-left "
        \            . "bit-shift-right symbol keyword gensym count conj seq "
        \            . "first rest ffirst fnext nfirst nnext second every? "
        \            . "not-every? some not-any? concat reverse cycle "
        \            . "interleave interpose split-at split-with take "
        \            . "take-nth take-while drop drop-while repeat replicate "
        \            . "iterate range into distinct sort sort-by zipmap "
        \            . "line-seq butlast last nth nthnext next "
        \            . "repeatedly tree-seq enumeration-seq iterator-seq "
        \            . "coll? associative? empty? list? reversible? "
        \            . "sequential? sorted? list list* cons peek pop vec "
        \            . "vector peek pop rseq subvec array-map hash-map "
        \            . "sorted-map sorted-map-by assoc assoc-in dissoc get "
        \            . "get-in contains? find select-keys update-in key val "
        \            . "keys vals merge merge-with max-key min-key "
        \            . "create-struct struct-map struct accessor "
        \            . "remove-method meta with-meta in-ns refer create-ns "
        \            . "find-ns all-ns remove-ns import ns-name ns-map "
        \            . "ns-interns ns-publics ns-imports ns-refers ns-resolve "
        \            . "resolve ns-unmap name namespace require use "
        \            . "set! find-var var-get var-set ref deref "
        \            . "ensure alter ref-set commute agent send send-off "
        \            . "agent-errors clear-agent-errors await await-for "
        \            . "instance? bean alength aget aset aset-boolean "
        \            . "aset-byte aset-char aset-double aset-float "
        \            . "aset-int aset-long aset-short make-array "
        \            . "to-array to-array-2d into-array int long float "
        \            . "double char boolean short byte parse add-classpath "
        \            . "cast class get-proxy-class proxy-mappings "
        \            . "update-proxy hash-set sorted-set set disj set? "
        \            . "aclone add-watch alias alter-var-root "
        \            . "ancestors await1 bases bigdec bigint bit-and-not "
        \            . "bit-clear bit-flip bit-set bit-test counted?"
        \            . "char-escape-string char-name-string class? "
        \            . "compare compile construct-proxy delay? "
        \            . "derive descendants distinct? double-array "
        \            . "doubles drop-last empty float-array floats "
        \            . "force gen-class get-validator int-array ints "
        \            . "isa? long-array longs make-hierarchy method-sig "
        \            . "not-empty ns-aliases ns-unalias num partition "
        \            . "parents pmap prefer-method primitives-classnames "
        \            . "print-ctor print-dup print-method print-simple "
        \            . "proxy-call-with-super "
        \            . "proxy-super rationalize read-string remove "
        \            . "remove-watch replace resultset-seq rsubseq "
        \            . "seque set-validator! shutdown-agents subseq "
        \            . "supers "
        \            . "unchecked-add unchecked-dec unchecked-divide "
        \            . "unchecked-inc unchecked-multiply unchecked-negate "
        \            . "unchecked-subtract underive xml-seq trampoline "
        \            . "atom compare-and-set! ifn? gen-interface "
        \            . "intern init-proxy io! memoize proxy-name swap! "
        \            . "release-pending-sends the-ns unquote while "
        \            . "unchecked-remainder alter-meta! "
        \            . "future-call methods mod pcalls prefers pvalues "
        \            . "reset! realized? some-fn "
        \            . "reset-meta! type vary-meta unquote-splicing "
        \            . "sequence clojure-version counted? "
        \            . "chunk-buffer chunk-append chunk chunk-first "
        \            . "chunk-rest chunk-next chunk-cons chunked-seq? "
        \            . "deliver future? future-done? future-cancel "
        \            . "future-cancelled? get-method promise "
        \            . "ref-history-count ref-min-history ref-max-history "
        \            . "agent-error assoc!  boolean-array booleans bound-fn* "
        \            . "bound?  byte-array bytes char-array char? chars "
        \            . "conj!  denominator disj!  dissoc!  error-handler "
        \            . "error-mode extenders extends?  find-protocol-impl "
        \            . "find-protocol-method flatten frequencies "
        \            . "get-thread-bindings group-by hash-combine juxt "
        \            . "munge namespace-munge numerator object-array "
        \            . "partition-all partition-by persistent! pop! "
        \            . "pop-thread-bindings push-thread-bindings rand-nth "
        \            . "reductions remove-all-methods restart-agent "
        \            . "satisfies?  set-error-handler!  set-error-mode! "
        \            . "short-array shorts shuffle sorted-set-by take-last "
        \            . "thread-bound? transient vector-of with-bindings* fnil "
        \            . "spit biginteger every-pred find-keyword "
        \            . "unchecked-add-int unchecked-byte unchecked-char "
        \            . "unchecked-dec-int unchecked-divide-int "
        \            . "unchecked-double unchecked-float "
        \            . "unchecked-inc-int unchecked-int unchecked-long "
        \            . "unchecked-multiply-int unchecked-negate-int "
        \            . "unchecked-remainder-int unchecked-short "
        \            . "unchecked-subtract-int with-redefs with-redefs-fn"
        \ }

    for category in keys(s:builtins_map)
        let words = split(s:builtins_map[category], " ")
        let words = map(copy(words), '"clojure.core/" . v:val') + words
        let s:builtins_map[category] = words
    endfor

    call s:ColorNamespace(s:builtins_map)


    syn cluster clojureAtomCluster   contains=clojureError,clojureFunc,clojureMacro,clojureCond,clojureDefine,clojureRepeat,clojureException,clojureConstant,clojureVariable,clojureSpecial,clojureKeyword,clojureString,clojureCharacter,clojureNumber,clojureBoolean,clojureQuote,clojureUnquote,clojureDispatch,clojurePattern
    syn cluster clojureTopCluster    contains=@clojureAtomCluster,clojureComment,clojureSexp,clojureAnonFn,clojureVector,clojureMap,clojureSet

    syn keyword clojureTodo contained FIXME XXX TODO FIXME: XXX: TODO:
    syn match   clojureComment contains=clojureTodo ";.*$"

    syn match   clojureKeyword "\c:\{1,2}[a-z?!\-_+*./=<>#$][a-z0-9?!\-_+*\./=<>#$]*"

    syn region  clojureString start=/L\="/ skip=/\\\\\|\\"/ end=/"/

    syn match   clojureCharacter "\\."
    syn match   clojureCharacter "\\[0-7]\{3\}"
    syn match   clojureCharacter "\\u[0-9]\{4\}"
    syn match   clojureCharacter "\\space"
    syn match   clojureCharacter "\\tab"
    syn match   clojureCharacter "\\newline"
    syn match   clojureCharacter "\\return"
    syn match   clojureCharacter "\\backspace"
    syn match   clojureCharacter "\\formfeed"

    let radixChars = "0123456789abcdefghijklmnopqrstuvwxyz"
    for radix in range(2, 36)
        execute 'syn match clojureNumber "\c\<-\?' . radix . 'r['
                    \ . strpart(radixChars, 0, radix)
                    \ . ']\+\>"'
    endfor

    syn match   clojureNumber "\<-\=[0-9]\+\(\.[0-9]*\)\=\(M\|\([eE][-+]\?[0-9]\+\)\)\?\>"
    syn match   clojureNumber "\<-\=[0-9]\+N\?\>"
    syn match   clojureNumber "\<-\=0x[0-9a-fA-F]\+\>"
    syn match   clojureNumber "\<-\=[0-9]\+/[0-9]\+\>"

    syn match   clojureQuote "\('\|`\)"
    syn match   clojureUnquote "\(\~@\|\~\)"
    syn match   clojureDispatch "\(#^\|#'\)"

    syn match   clojureAnonArg contained "%\(\d\|&\)\?"
    syn match   clojureVarArg contained "&"

    syn region  clojureAnonFn  matchgroup=clojureParen0 start="#(" matchgroup=clojureParen0 end=")"  contains=@clojureTopCluster,clojureAnonArg,clojureSexpLevel0
    syn region  clojureVector  matchgroup=clojureParen0 start="\[" matchgroup=clojureParen0 end="\]" contains=@clojureTopCluster,clojureVarArg,clojureSexpLevel0
    syn region  clojureMap     matchgroup=clojureParen0 start="{"  matchgroup=clojureParen0 end="}"  contains=@clojureTopCluster,clojureSexpLevel0
    syn region  clojureSet     matchgroup=clojureParen0 start="#{" matchgroup=clojureParen0 end="}"  contains=@clojureTopCluster,clojureSexpLevel0

    syn region  clojurePattern start=/L\=\#"/ skip=/\\\\\|\\"/ end=/"/

    syn region  clojureCommentSexp                          start="("                                       end=")" transparent contained contains=clojureCommentSexp
    syn region  clojureComment     matchgroup=clojureParen0 start="(comment"rs=s+1 matchgroup=clojureParen0 end=")"                       contains=clojureCommentSexp
    syn region  clojureComment                              start="#!" end="\n"
    syn match   clojureComment "#_"

    syn sync fromstart

    if version >= 600
        command -nargs=+ HiLink highlight default link <args>
    else
        command -nargs=+ HiLink highlight         link <args>
    endif

    HiLink clojureConstant  Constant
    HiLink clojureBoolean   Boolean
    HiLink clojureCharacter Character
    HiLink clojureKeyword   Operator
    HiLink clojureNumber    Number
    HiLink clojureString    String
    HiLink clojurePattern   Constant

    HiLink clojureVariable  Identifier
    HiLink clojureCond      Conditional
    HiLink clojureDefine    Define
    HiLink clojureException Exception
    HiLink clojureFunc      Function
    HiLink clojureMacro     Macro
    HiLink clojureRepeat    Repeat

    HiLink clojureQuote     Special
    HiLink clojureUnquote   Special
    HiLink clojureDispatch  Special
    HiLink clojureAnonArg   Special
    HiLink clojureVarArg    Special
    HiLink clojureSpecial   Special

    HiLink clojureComment   Comment
    HiLink clojureTodo      Todo

    HiLink clojureError     Error

    HiLink clojureParen0    Delimiter

else " using slimv impl
    echomsg "using slimv implementation"

    " Clojure keywords not defined by lisp.vim
    syn keyword lispFunc def defmulti defn defonce defprotocol doall dorun doseq dosync doto
    syn keyword lispFunc filter fn for future in-ns letfn ns range str take try

    runtime syntax/**/lisp.vim


    " Add [] and {} to the lisp_rainbow handling
    syn match			 lispSymbol			  contained			   ![^()\[\]{}'`,"; \t]\+!
    syn match			 lispBarSymbol			  contained			   !|..\{-}|!
    if exists("g:lisp_rainbow") && g:lisp_rainbow != 0
        syn region lispParen0           matchgroup=hlLevel0 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen1 
        syn region lispParen1 contained matchgroup=hlLevel1 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen2 
        syn region lispParen2 contained matchgroup=hlLevel2 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen3 
        syn region lispParen3 contained matchgroup=hlLevel3 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen4 
        syn region lispParen4 contained matchgroup=hlLevel4 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen5 
        syn region lispParen5 contained matchgroup=hlLevel5 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen6 
        syn region lispParen6 contained matchgroup=hlLevel6 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen7 
        syn region lispParen7 contained matchgroup=hlLevel7 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen8 
        syn region lispParen8 contained matchgroup=hlLevel8 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen9 
        syn region lispParen9 contained matchgroup=hlLevel9 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen0

        syn region lispParen0           matchgroup=hlLevel0 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen1 
        syn region lispParen1 contained matchgroup=hlLevel1 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen2 
        syn region lispParen2 contained matchgroup=hlLevel2 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen3 
        syn region lispParen3 contained matchgroup=hlLevel3 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen4 
        syn region lispParen4 contained matchgroup=hlLevel4 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen5 
        syn region lispParen5 contained matchgroup=hlLevel5 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen6 
        syn region lispParen6 contained matchgroup=hlLevel6 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen7 
        syn region lispParen7 contained matchgroup=hlLevel7 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen8 
        syn region lispParen8 contained matchgroup=hlLevel8 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen9 
        syn region lispParen9 contained matchgroup=hlLevel9 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen0
    endif
end
