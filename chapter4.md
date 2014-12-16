#1. 基本的な再帰
次の関数群を、再帰を用いて定義せよ。  
なお非負整数を受け取る関数については、非負整数を受け取った場合の挙動を考慮しなくて良い。

  1. `tri_number` : 非負整数`n`を受け取り、`0`から`n`までの総和を求める関数(これを **三角数** という)
  2. `tetration` : 整数`n`と非負整数`m`を受け取り、`n`のべき乗を`m`回繰り返した値を求める関数(これを **テトレーション** という)  
    例えば、`tetration 2 4`は`2^2^2^2`を計算する  
    なお、`^`は右結合の演算子である(`tetration 2 4 = 2^(2^(2^2)) = 65536`)
  3. `index` : 非負整数`n`とリスト`xs`を受け取り、`xs`の`n`番目の要素を返す関数  
    ただし、先頭要素を0番目とし、範囲外の場合はエラーとすること
  4. `even_odd` : 整数のリスト`xs`を受け取り、奇数の要素のリストと偶数の要素のリストのペアを返す関数  
    ただし、要素間の順序は保持すること

###例
```haskell
*Main> :t tri_number
tri_number :: Int -> Int
*Main> take 10 [tri_number n | n <- [1..]]
[1,3,6,10,15,21,28,36,45,55]

*Main> -- 非常に大きな数になるためIntegerを用いる
*Main> :t tetration
tetration :: Integer -> Integer -> Integer
*Main> take 5 [tetration 2 n | n <- [0..]]
[1,2,4,16,65536]
*Main> take 4 [tetration 3 n | n <- [0..]]
[1,3,27,7625597484987]
*Main> -- tetration 2 5 や tetration 3 4 は非常に大きな数になるため注意せよ

*Main> :t index
index :: Int -> [a] -> a
*Main> index 0 [2,4,6,8,10]
2
*Main> index 2 [2,4,6,8,10]
6
*Main> index 5 [2,4,6,8,10]
*** Exception: index outbounded

*Main> :t even_odd
even_odd :: [Int] -> ([Int], [Int])
*Main> even_odd [2,8,1,4,5,3,9,6]
([1,5,3,9],[2,8,4,6])
```

#2. 挿入ソート
### 挿入ソートの定義
整列済みのリスト`xs`に、新しい値`x`を追加することを考えよう。  
このとき、`xs`の適切な位置に`x`を挿入することで、新たなリストも整列済みであるようにすることができる。  
例えば、`[1,3,4,6]`に`2`を挿入する場合、`1`と`3`の間に挿入すれば、`[1,2,3,4,6]`という新たな整列済みのリストを得ることが出来る。  

ところで、空のリストは整列済みであるとみなすことが出来る。  
このため、空リストに繰り返し「適切な挿入」を繰り返すことで、整列済みのリストを得ることが出来る。  
例えば、`[4,2,3,1]`という未ソートのリストがあった場合、以下の手順でソート済みのリストを得る。
  - `[]`に`4`を挿入して、`[4]`を得る
  - `[4]`に`2`を挿入して、`[2,4]`を得る
  - `[2,4]`に`3`を挿入して、`[2,3,4]`を得る
  - `[2,3,4]`に`1`を挿入して、`[1,2,3,4]`を得る

このようなソートアルゴリズムを **挿入ソート** と呼ぶ。

### 問
以下の関数を、再帰を用いて定義せよ。
  1. `insert` : 整列済みリストと挿入する値を受け取り、要素を挿入した整列済みリストを返す関数
  2. `isort` : リストを受け取り、`insert`を用いた挿入ソートを行い整列済みリストを返す関数

### 例
```haskell
*Main> :t insert
insert :: Ord a => [a] -> a -> [a]
*Main> insert [1,3,5] 4
[1,3,4,5]
*Main> insert [1,3,5] 0
[0,1,3,5]
*Main> insert [1,3,5] 6
[1,3,5,6]

*Main> :t isort
isort :: Ord a => [a] -> [a]
*Main> isort [4,2,3,1,5]
[1,2,3,4,5]
```

#3. 分割数
*難易度の高いトピックである*
### 分割数の定義
正整数を、重複を許す正整数の和で表すことを考える。  
例えば`4`は
  - `1+1+1+1`
  - `2+1+1`
  - `3+1`
  - `2+2`
  - `4`

の5通りの表現がある。(順序の違いは無視する。例えば`1+3`のようなパターンは`3+1`に含まれる)

正整数`n`について、このように重複を許す正整数の和で表したものを`n`の **分割** と呼び、分割の種類数を`n`の **分割数** と呼ぶ。  
例えば`4`は先ほど挙げた通り5通りの分割があるため、`4`の分割数は`5`である。

### 問
次の関数を定義せよ。  
ただし、ゼロ及び負数を受け取った場合の挙動は考慮しなくて良い。
  - `part_num` : 正整数を受け取り、その数の分割数を返す関数

### 例
```haskell
*Main> :t part_num
part_num :: Int -> Int
*Main> take 10 [part_num n | n <- [1..]]
[1,2,3,5,7,11,15,22,30,42]
```

### ヒント
2つの正整数`n`と`m`を受け取り、`m`以上の数のみを使った`n`の分割の種類を返すような補助関数`part_num'`を定義するとよい。  
このとき、「`m`を少なくとも一つ用いる」か、「`m`を一つも用いないか」に着目し再帰を適用せよ。
例えば4の分割のうち、1を含むものは
  - `1+1+1+1`
  - `2+1+1`
  - `3+1`

の3種類、1を含まないものは
  - `2+2`
  - `4`

の2種類である。  
1から5程度までのそれぞれの数について、その分割を列挙し、これらの間にある関係を考察せよ。

### 参考
分割数は上述の通り単純に構成される数であるが、その一般項は非常に複雑になる。  
参考 : http://ameblo.jp/interesting-math/entry-10811088826.html

分割数についての分析の話題は「[数学ガール](http://amzn.to/1qzTUS6)」第10章が詳しい。  
ただし当該章では母関数なども扱うため、前章を通して読むことが望ましい。