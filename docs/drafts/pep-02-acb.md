PEP-02:  acb-and-initial-encrypting-scheme


Задача: шифровать контент таким образом, что бы его
могло получать несколько [1..n], n ~ в перспективе сотни?
получателей.

Предлагаемое решение:

Вводим ACB, Access Control Block.

ACB определяет права различных подписчиков на единицу контента.

Ключи шифрования контента определяется из ACB.

На первоначальном этапе предлагается использовать пару ключей
Curve25519 (?), т.е ключевую пару асимметричного шифрования libsodium
(haskell: saltine), так как тесты показали заметно более высокую
скорость по сравнению с шифрованием симметричным ключом (AES).

В любом случае, предполагается в будущем использовать и другие схемы
контроля доступа к контенту, таким образом, возможность расширения
должна быть заложена в структуры данных.


```

-- PubKey 'Sign e ;;; ключ подписи Ed25519

data ACBSchema = NaClAsymm

data family ACB e (schema :: ACBSchema) :: Type

data family AccessKey e schema :: Type


data instance ACB e 'NaClAsymm  =
  ACBNaClAsymm
  { acbParent   :: HashRef              -- указатель на предыдущий ACB
  , acbRoot     :: PubKey 'Sign e       -- корневой владелец
  , acbOwners   :: [PubKey 'Sign e]     -- ключи владельцев

  , acbRead     :: [(PubKey 'Sign e, PubKey 'Encrypt e)]
     -- при чтении нужно расшифровывать и идентифицировать ключ

  , acbWrite    :: [(PubKey 'Sign e)]
     -- при публикации нужно проверять подпись
  }

```

Что бы подписчик мог что-то публиковать (write), мы должны знать его
публичный ключ подписи, что бы её проверять и принимать, или
отвергать данные.

Что бы подписчик мог что-то читать, мы должны знать его публичный ключ
шифрования, что бы он мог расшифровать публикуемый контент.

ACB не является закрытой информацией и может быть как опубликован,
так и сохранён локально. Утечка ACB не является проблемой
за исключением раскрытия принадлежности ключа шифрования к ключу
подписи.


```

-- EncryptedBox - обертка вокруг ключа ассиметричного шифрования
--                (KeyPAir)

newtype instance AccessKey e 'NaClAsymm =
  AccessKeyNaClAsymm
  { permitted :: [(PubKey 'Sign e, EncryptedBox)]
  }

```

Список пар (ключ подписи пользователя, ключ шифрования).
Пара необходима, что бы пользователи за O(1) найти и
расшифровать свой ключ.


Операции:

  - Добавить право на чтение
  - Добавить право на публикацию
  - Удалить право на чтение
  - Удалить право на публикацию
  - Добавить владельца
  - Удалить владельца
  - Создать новый ключ публикации


### Создать новый ключ публикации

  - Получить ACB
  - Для каждого подписчика на чтение добавить зашифрованный
    ключ в permitted


## Публикация

  - Взять ключ публикации
  - Расшифровать свой экземпляр ключа (KeyPair)
  - Шифровать расшифрованным ключом блоки

## Чтение

 - Определить ключ публикации
 - Взять ключ публикации
 - Расшифровать свой экземпляр ключа (KeyPair)
 - Расшифровывать этим ключом блоки

