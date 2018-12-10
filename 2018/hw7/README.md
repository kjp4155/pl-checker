# HW7
## How to use
### 7-1 : let-polymorphic M type checker (`ex1/`)
1. 구현한 `poly_checker.ml`을 복사하거나 뼈대코드 `poly_checker.orig.ml`을 `poly_checker.ml`로 복사 한 후 구현합니다.
2. `./check` 명령어를 통해 테스트케이스를 실행합니다.

- `examples/<test>.m` : 테스트케이스
- `examples/<test>.ans` : 정답
- `examples/<test>.out` : 출력
- `examples/<test>.err` : 에러

### 7-2 : Exception is Sugar (`ex2/`)
1. 구현한 `desugar.ml`을 복사하거나 뼈대코드 `desugar.orig.ml`을 `desugar.ml`로 복사 한 후 구현합니다.
2. `./check` 명령어를 통해 테스트케이스를 실행합니다.

- `examples/<test>.m` : 테스트케이스
- `examples/<test>.out` : 결과
- `examples/<test>.err` : 에러

변환 전 프로그램이 unhandled exception을 낼 경우 201812를 결과로 내야함에 유의하세요.
