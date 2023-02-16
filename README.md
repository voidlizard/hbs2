


## How to launch a peer

Example:
```

hbs2-peer run -p .peers/1  -k .peers/1/key -l addr:port -r rpcaddr:rpcport

```


## Как сохранять зашифрованный файл

```
keyring-new > kr
keyring-list kr
; создаём файл со списком публичных ключей
; строчки из выхлопа команды keyring-list
groupkey-new path/to/file/with/list/of/pubkeys > groupkey
store --groupkey groupkey file/to/store
; получаем хэш
cat --keyring kr <хэш>
```
