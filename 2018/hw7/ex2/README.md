> SNU 4190.310 Programming Languages

# Homework "Exception is Sugar"

## 컴파일 및 실행 방법
`xexp.ml`에는 xexp 언어와 그 실행기가 정의되어 있습니다. `desugar.ml` 파일에는 주어진 프로그램에서 설탕 문법구조 (syntactic sugar)인 `raise`와 `handle` 녹여 없애는 `removeExn` 함수가 선언되어 있습니다. 이 함수를 구현하는 것이 이번 과제에서 할 일입니다.


아래와 같이 실행하면, 주어진 xexp 프로그램을 여러분이 작성한 `removeExn` 함수로 변환하여 실행합니다. `removeExn` 함수로 변환한 결과에 여전히 `raise`와 `handle`이 남아있을 경우, 에러 메시지도 함께 출력해 줍니다.

```
$ make
$ ./run examples/test1.xexp
```

>>> TIP.
실행시, 파일명을 명시하지 않을 경우, 표준입력으로부터 실행코드를 읽어들입니다.  표준 입력으로 프로그램을 입력하신 후, 첫 번째 칸(column)에서 Ctrl-D 를 누르시면 프로그램이 실행됩니다.


## 변환된 프로그램 출력하기
`-pdesug` 옵션을 주어 변환된 프로그램을 출력할 수 있습니다.

 ```
$ make
$ ./run -pdesug examples/test1.xexp
```

## 숙제 제출 관련
`desugar.ml` 파일에 있는 `removeExn` 함수를 완성하시고 그 파일만 제출해 주세요.


---
15최재승 <jschoi@ropas.snu.ac.kr>
17이동권 <dklee@ropas.snu.ac.kr>
18이동권, 배요한<dklee, yhbae@ropas.snu.ac.kr>
