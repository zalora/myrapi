### Haskell client for Myracloud API 

Work in progress.

```console
export MYRA_ACCESS_KEY=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
export MYRA_SECRET_KEY=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

ghci -XOverloadedStrings Myracloud.hs
> myra_ $ ListDNSRecords "example.com"
```
