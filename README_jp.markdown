# Butter

Butter は Common Lisp のユニットテスティングフレームワークです。Clojure の clojure.test に似ています。

## 使い方

    (defpackage :your-package
      (:use :cl
            :butter))
    (in-package :your-package)

    (deftest first-test-case
      (is (= 10 (+ 1 2 3 4)))
      (is (equal '(garlic butter) (append '(garlic) '(butter)))))

    (deftest second-test-case
      (is (oddp 11) "11 is an odd number.")
      (is (evenp 12) "12 is an even number."))

    (run-tests)

## アサーション

*is* マクロを使って式に対するアサーションを書くことができます。

    (is (= 10 (+ 1 2 3 4)))
    (is (equal '(a b c) (cons 'a '(b c))))
    (is (keywordp :x))

*is* マクロは REPL  から直接使うこともできます。

    > (is (= 10 (+ 1 2 3 4)))
    T

アサーションが失敗したときには以下のようなメッセージが表示されます。

    > (is (= 8 (+ 2 4)))
    FAIL in TOP LEVEL
    expected: (= 8 (+ 2 4))
      actual: (NOT (= 8 6))
    NIL

expected は *is* マクロに渡した式を表し、actual は実際に起こったことを表します。

アサーションが成功した場合、 *is* マクロは T を返し、失敗した場合は NIL を返します。

### 特別なアサーション

2つの特別なアサーションを用意しています。

#### :signal

*:signal* はコンディションが通知されたかどうかをテストします。

    (is (:signal type-error (length 'a)))

*:signal* はコンディションが通知された時点で即座にアサーションを成功させます。

#### :print

*:print* は指定されたストリームへの出力の内容をテストします。

    (is (:print *standard-output* "garlic butter!" (format t "~A butter!" "garlic")))

*butter.extending* パッケージの *define-special-assertion* マクロを使って特別なアサーションを追加できます。

### are マクロ

*are* マクロを使うと複数のアサーションを簡単に書けます。

    (are (expected x y) `(= ,expected (+ ,x ,y))
      (10 3 7)
      (15 8 7)
      (9 6 3))

このフォームは以下のように展開されます。

    (progn
      (is (= 10 (+ 3 7)))
      (is (= 15 (+ 8 7)))
      (is (= 9 (+ 6 3))))

## テストの定義

*deftest* マクロでひとつのまとまったテストを現在のパッケージに定義できます。

    (deftest appending-lists
      (is (equal '(1 2 3 4) (append '(1 2) '(3 4))))
      (is (equal '(a b c) (append '(a b c) ())))
      (is (equal '(garlic butter) (append () '(garlic butter)))))
    
    (deftest concatenating-strings
      (is (string= "ABCDE" (concatenate 'string "ABC" "DE")))
      (is (string= "GARLIC BUTTER" (concatenate 'string "GARLIC" " " "BUTTER"))))

## テストの実行

*run-tests* 関数で、指定したパッケージのテストを実行できます。

    (run-tests :your-package :other-package)

パッケージが指定されなかった場合は、現在のパッケージのテストを実行します。

*run-all-tests* 関数ですべてのパッケージのすべてのテストを実行できます。

    (run-all-tests)

## テストレポーター

テストの結果はテストレポーターが出力します。デフォルトのテストレポーターは結果を \*standard-output\* に出力します。*with-reporter* マクロを使うとテストレポーターを変更できます。

    (with-reporter (make-instance 'butter.cui:cui-reporter :verbose t)
      (run-tests))

*butter.cui* パッケージの *cui-reporter* クラスは結果をストリームに出力します。initarg を指定することで、振る舞いを変えられます。

  - stream - 出力先のストリームです。デフォルトは \*standard-output\* です。
  - verbose - NIL でない場合は成功したアサーションについての出力も行います。NIL の場合は失敗したものだけを表示します。デフォルトは NIL です。
  - invoke-debugger - NIL でない場合はエラーが発生した際にデバッガを起動します。NIL の場合はアサーションを失敗させます。デフォルトは NIL です。

## Butter の拡張

### 特別なアサーションの追加

*define-special-assertion* マクロを使って特別なアサーションを追加できます。*defmacro* と同じように書きます。

    (define-special-assertion my-eql (expected form)
      (let ((value-symbol (gensym)))
        `(let ((,value-symbol ,form))
           (values (eql ,value-symbol ,expected) ,value-symbol))))

上記は *my-eql* という特別なアサーションを作成します。これは *is* マクロ内で使うことができます。

    (is (my-eql 10 (+ 3 7)))

特別なアサーションの展開結果の式は、2つの値を返す必要があります。ひとつ目は、アサーションが成功した場合に NIL でない値、失敗した場合には NIL になります。ふたつ目は、実際に何が起こったかを表す値です。
