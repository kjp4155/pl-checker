# Notice 
- HW2 테스트를 많이 보충했습니다. pull 해 주세요.
- HW2 체커가 완성되었습니다! 아직은 데이터가 약합니다. 제출기한 전까지 수시로 보충할 예정입니다.
- HW1 테스트 업데이트가 완료되었습니다. (TA 예제 포함)
- 2017, 2018과제를 구분하도록 디렉토리 구조를 수정했습니다.
- "./check 2" 와 같이 과제 번호를 지정해 주어야 어떤 테스트 케이스에서 틀렸는지 확인이 가능합니다.

# pl-checker
2018년 가을학기 Programming Language 숙제 자동 채점기 <br>
[Jhuni의 채점기](https://github.com/Jhuni0123/pl-checker) 에 기반해서 만들어진 채점기입니다.<br>

채점의 **자동화**를 목표로 하고있습니다. 최대한 많은 경우를 테스트 하게 하려고 노력하고 있지만 커버되지 않는 케이스가 있을 수 있습니다. 즉 여기 있는 테스트 케이스를 **모두 통과해도 만점을 받는것을 보장하지 못합니다.** <br>
좋은 테스트케이스가 있다면 [이슈보드](https://github.com/kjp4155/pl-checker/issues)에 올려주신다면 최대한 빠르게 추가하겠습니다.

# How to use
unix 기반 환경을 필요로 합니다
```bash
git clone https://github.com/kjp4155/pl-checker
cd pl-checker
cd ./2018/hw<num>
# please read README.md
./check
```

# How to update
**테스트케이스가 종종 업데이트 됩니다. 주기적으로 업데이트 해주세요!** <br>
업데이트 해도 작성한 파일이 덮어씌워지지 않습니다.

```bash
git pull --rebase origin master
```
<!--
# Status
- HW 7-1 : TA testcases
- HW 7-2, 7-3 : TBD
-->

# Thanks to
- [Jhuni](https://github.com/Jhuni0123)
- [kipa00](https://github.com/kipa00)
- All students sharing testcases
- All TAs in PL lecture

---
[MIT License](LICENSE)
