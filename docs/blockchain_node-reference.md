# blockchain_node

An API to the Helium blockchain-node.
This api follows the json-rpc 2.0 specification. More information available at http://www.jsonrpc.org/specification.

<strong>Version 1.0</strong>

---

- [block_height](#block_height)
- [block_get](#block_get)
- [acccount_get](#acccount_get)
- [transaction_get](#transaction_get)
- [oracle_price_current](#oracle_price_current)
- [oracle_price_get](#oracle_price_get)
- [pending_transaction_status](#pending_transaction_status)
- [pending_transaction_submit](#pending_transaction_submit)
- [wallet_create](#wallet_create)
- [wallet_delete](#wallet_delete)
- [wallet_list](#wallet_list)
- [wallet_unlock](#wallet_unlock)
- [wallet_lock](#wallet_lock)
- [wallet_is_locked](#wallet_is_locked)
- [wallet_pay](#wallet_pay)
- [wallet_pay_multi](#wallet_pay_multi)
- [wallet_import](#wallet_import)
- [wallet_export](#wallet_export)
- [wallet_backup_list](#wallet_backup_list)
- [wallet_backup_create](#wallet_backup_create)
- [wallet_backup_delete](#wallet_backup_delete)
- [wallet_backup_restore](#wallet_backup_restore)

---

<a name="block_height"></a>

## block_height

Gets the stored height of the blockchain.

### Description

Gets the stored height of the blockchain.

### Result

| Name   | Type   | Constraints | Description  |
| ------ | ------ | ----------- | ------------ |
| result | number |             | Block height |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "block_height"
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": 318492
}
```

<a name="block_get"></a>

## block_get

Get a block by height or hash.

### Description

Gets a block with it's transaction hashes given a block height or block hash.

### Parameters

| Name           | Type   | Constraints | Description           |
| -------------- | ------ | ----------- | --------------------- |
| params         | object |             |                       |
| params?.height | number |             | Block height to fetch |
| params?.hash   | number |             | Block hash to fetch   |

### Result

| Name                  | Type   | Constraints | Description                         |
| --------------------- | ------ | ----------- | ----------------------------------- |
| result                | object |             | Block details                       |
| result.hash           | string |             | Hash of block                       |
| result.height         | number |             | Height of block                     |
| result.prev_hash      | string |             | Hash of previous block              |
| result.time           | number |             | Time of block in seconds from epoch |
| result.transactions   | array  |             | Block transaction hashes            |
| result.transactions[] | string |             | Transaction hash                    |

### Errors

| Code  | Message | Description         |
| ----- | ------- | ------------------- |
| -100  |         | Block not found     |
| -150  |         | Failed to get block |
| -3602 |         | Invalid parameter   |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "block_get",
  "params": {
    "height": 318492
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {
    "hash": "vX_PzD2DvIQZPlpM_LiDCCewpWuZkwcdAhjnJXeg5Gk",
    "height": 318492,
    "prev_hash": "OLv5ah-94zg3ySJK5x50-W6Kw4gd510ikhpbByq37ZU",
    "time": 1588558709,
    "transactions": [
      "wexZNzv5nlLI9riuN8WDaBS8yxhGwfFBYK4MVSGDHis",
      "nBifAlkm6d6b2kPclJbKI8T4rjefgwhsZxKPUHXFJSM",
      "k_ZS64CexnpvYfwQLjNytKBHrGrZMe8hI_lHpyD6Tx0",
      "ER_N9N58gsIkPubq0tSlrWZUtdD1ij8TsEqMZDGQuSI",
      "bsnMBjroAfiuyQe1lI24zGTeqtIKKBDPbqlLokgxQg8",
      "39Sj-bprwSiI28DMFZid0pXsTqEvS-tTZa6zMyyE_ig",
      "sqMRtZLeBFgOpvB_tOOa2H1UVXNFJc-9K8qYfdJMRME",
      "gPbkpNBhuQrZAbLjkkeUlVBjXMHTWk5AJn0GEK8GRgE",
      "pnDJ1K5ydeABq3Zy1xV_ey0TF3FouDScqKk1lPqnUsU",
      "Y3sNXBPfmt1SRPhXbG3mK9W2kEAvnFWBLb77ATADmv0",
      "utKCgooy2wb6EK6n8S-hln16ugZhwnUdfRUWv6MOBGE",
      "-Zj-qfR1VB0NyIUavkZgr5qzPDxkIxIM1WZdvs1hkRk",
      "-BpXPSMylHwUbU3FtHzxMD_X5smuHqSi-d3DB0vllmQ",
      "t6BWSO0nIvIO_wpiJOjCMd7hO3rIWSapF-p6IybQAjg",
      "yCFtd4m34B8PNHOsFH708bPHKW9JuWe3iFoChkgpLdw",
      "yrbiQ4uPQQYjBJN5YOc73CkBWx3Y9354sHymKqwWJUA",
      "TsGVx5z8Ozk0jHGfBS0heK8tBpcHjSgyzvD4rkCbnKk",
      "NrS5Q4RCIuECKt0cb3mrnPdLQtrS9hSQudiZAl5ZObI",
      "4lsxf5RUK66CrLcG8CqCUNUq5V5onvz11FZPHR2QLzI",
      "Mw080p4DaYLW0xhRJh4fvnfDfJ7I1Iz4idqUtTZwdu4"
    ]
  }
}
```

<a name="acccount_get"></a>

## acccount_get

Get account details.

### Description

Get account details for a given account address.

### Parameters

| Name           | Type   | Constraints | Description                         |
| -------------- | ------ | ----------- | ----------------------------------- |
| params         | object |             |                                     |
| params.address | string |             | B58 address of the account to fetch |

### Result

| Name               | Type   | Constraints | Description                                      |
| ------------------ | ------ | ----------- | ------------------------------------------------ |
| result             | object |             | Account                                          |
| result.address     | string |             | Address of the account                           |
| result.balance     | number |             | HNT balance of the account in bones              |
| result.nonce       | number |             | The current nonce for the account                |
| result.dc_balance  | number |             | Data credit balance of the account               |
| result.dc_nonce    | number |             | The current data credit nonce for the account    |
| result.sec_balance | number |             | Security token balance of the account            |
| result.sec_nonce   | number |             | The current security token nonce for the account |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "acccount_get",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {
    "address": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5",
    "balance": 1000,
    "nonce": 3,
    "dc_balance": 0,
    "dc_nonce": 0,
    "sec_balance": 0,
    "sec_nonce": 0
  }
}
```

<a name="transaction_get"></a>

## transaction_get

Get transaction details.

### Description

Get details for a given transaction hash.

### Parameters

| Name        | Type   | Constraints | Description                          |
| ----------- | ------ | ----------- | ------------------------------------ |
| params      | object |             |                                      |
| params.hash | string |             | B64 hash of the transaction to fetch |

### Result

| Name        | Type   | Constraints | Description                                                                                           |
| ----------- | ------ | ----------- | ----------------------------------------------------------------------------------------------------- |
| result      | object |             | Transaction details. The exact fields returned depend on the transaction type returned in the result. |
| result.hash | string |             | B64 hash of the transaction                                                                           |
| result.type | string |             | The type of the transaction                                                                           |

### Errors

| Code | Message | Description               |
| ---- | ------- | ------------------------- |
| -100 |         | Transaction not found     |
| -150 |         | Failed to get transaction |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "transaction_get",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {}
}
```

<a name="oracle_price_current"></a>

## oracle_price_current

Gets the current oracle price.

### Description

Gets the oracle price at the current height of the blockchain.

### Result

| Name          | Type   | Constraints | Description                              |
| ------------- | ------ | ----------- | ---------------------------------------- |
| result        | object |             | Oracle Price                             |
| result.price  | number |             | The oracle price at the indicated height |
| result.height | number |             | The block height of the oracle price     |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "oracle_price_current"
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {
    "price": 131069500,
    "height": 633936
  }
}
```

<a name="oracle_price_get"></a>

## oracle_price_get

Gets an oracle price at a height.

### Description

Gets the oracle price at the given height of the blockchain (if known).

### Parameters

| Name          | Type   | Constraints | Description                               |
| ------------- | ------ | ----------- | ----------------------------------------- |
| params        | object |             |                                           |
| params.height | number |             | Block height to get the oracle price for. |

### Result

| Name          | Type   | Constraints | Description                              |
| ------------- | ------ | ----------- | ---------------------------------------- |
| result        | object |             | Oracle Price                             |
| result.price  | number |             | The oracle price at the indicated height |
| result.height | number |             | The block height of the oracle price     |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "oracle_price_get",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {
    "price": 131069500,
    "height": 633936
  }
}
```

<a name="pending_transaction_status"></a>

## pending_transaction_status

Get pending transaction status.

### Description

Get the status a previously submitted transaction.

### Parameters

| Name        | Type   | Constraints | Description                                  |
| ----------- | ------ | ----------- | -------------------------------------------- |
| params      | object |             |                                              |
| params.hash | string |             | B64 hash of the pending transaction to fetch |

### Result

| Name   | Type   | Constraints | Description                                                  |
| ------ | ------ | ----------- | ------------------------------------------------------------ |
| result | string |             | One of 'pending', 'cleared', 'not_found' or a failure reason |

### Errors

| Code | Message | Description                   |
| ---- | ------- | ----------------------------- |
| -100 |         | Pending transaction not found |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "pending_transaction_status",
  "params": {
    "hash": "xG-KdomBEdp4gTiJO1Riif92DoMd5hPxadcSci05pIs"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": "cleared"
}
```

<a name="pending_transaction_submit"></a>

## pending_transaction_submit

Submit a transaction to the pending queue.

### Description

Submits a pending transaction to the pending queue. The transactions needs to be in a blockchain_txn envelope and base64 encoded

### Parameters

| Name       | Type   | Constraints | Description             |
| ---------- | ------ | ----------- | ----------------------- |
| params     | object |             |                         |
| params.txn | string |             | B64 encoded transaction |

### Result

| Name        | Type   | Constraints | Description                                                                                           |
| ----------- | ------ | ----------- | ----------------------------------------------------------------------------------------------------- |
| result      | object |             | Transaction details. The exact fields returned depend on the transaction type returned in the result. |
| result.hash | string |             | B64 hash of the transaction                                                                           |
| result.type | string |             | The type of the transaction                                                                           |

### Errors

| Code  | Message | Description       |
| ----- | ------- | ----------------- |
| -3602 |         | Invalid parameter |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "pending_transaction_submit",
  "params": {
    "txn": "{ 'txn': 'QoWBCIe...'"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {}
}
```

<a name="wallet_create"></a>

## wallet_create

Create a new wallet.

### Description

Creates a new wallet, encrypted with the given password. The wallet is locked after creation.

### Parameters

| Name            | Type   | Constraints | Description                         |
| --------------- | ------ | ----------- | ----------------------------------- |
| params          | object |             |                                     |
| params.password | string |             | Password used to encrypt the wallet |

### Result

| Name   | Type   | Constraints | Description                                  |
| ------ | ------ | ----------- | -------------------------------------------- |
| result | string |             | The B58 encoded public address of the wallet |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_create",
  "params": {
    "password": "a password"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5"
}
```

<a name="wallet_delete"></a>

## wallet_delete

Delets a wallet.

### Description

Permanently removes the wallet from the database.

### Parameters

| Name           | Type   | Constraints | Description                         |
| -------------- | ------ | ----------- | ----------------------------------- |
| params         | object |             |                                     |
| params.address | string |             | B58 address of the wallet to delete |

### Result

| Name   | Type    | Constraints | Description                            |
| ------ | ------- | ----------- | -------------------------------------- |
| result | boolean |             | Returns true if the wallet was deleted |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_delete",
  "params": {
    "address": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": true
}
```

<a name="wallet_list"></a>

## wallet_list

List all wallets.

### Description

Lists the public keys of all wallets.

### Result

| Name     | Type   | Constraints | Description                                |
| -------- | ------ | ----------- | ------------------------------------------ |
| result   | array  |             |                                            |
| result[] | string |             | The B58 encoded public address of a wallet |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_list"
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": ["13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5"]
}
```

<a name="wallet_unlock"></a>

## wallet_unlock

Unlock a wallet for signing.

### Description

Unlock a wallet for signing. The wallet will be unlocked for 60 seonds.

### Parameters

| Name            | Type   | Constraints | Description                         |
| --------------- | ------ | ----------- | ----------------------------------- |
| params          | object |             |                                     |
| params.address  | string |             | B58 address of the wallet to unlock |
| params.password | string |             | Password used to decrypt the wallet |

### Result

| Name   | Type    | Constraints | Description                            |
| ------ | ------- | ----------- | -------------------------------------- |
| result | boolean |             | Returns true if the wallet is unlocked |

### Errors

| Code | Message | Description      |
| ---- | ------- | ---------------- |
| -100 |         | Wallet not found |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_unlock",
  "params": {
    "address": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5",
    "password": "a password"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": true
}
```

<a name="wallet_lock"></a>

## wallet_lock

Lock a wallet.

### Description

Locks a previously unlocked wallet.

### Parameters

| Name           | Type   | Constraints | Description                       |
| -------------- | ------ | ----------- | --------------------------------- |
| params         | object |             |                                   |
| params.address | string |             | B58 address of the wallet to lock |

### Result

| Name   | Type    | Constraints | Description                                                   |
| ------ | ------- | ----------- | ------------------------------------------------------------- |
| result | boolean |             | Returns true regardless of whether the wallet is found or not |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_lock",
  "params": {
    "address": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": true
}
```

<a name="wallet_is_locked"></a>

## wallet_is_locked

Checks if a wallet is locked.

### Description

Checks if a wallet is unlocked.

### Parameters

| Name           | Type   | Constraints | Description                        |
| -------------- | ------ | ----------- | ---------------------------------- |
| params         | object |             |                                    |
| params.address | string |             | B58 address of the wallet to check |

### Result

| Name   | Type    | Constraints | Description                                    |
| ------ | ------- | ----------- | ---------------------------------------------- |
| result | boolean |             | Returns true if the wallet is locked or uknown |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_is_locked",
  "params": {
    "address": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": true
}
```

<a name="wallet_pay"></a>

## wallet_pay

Send a payment to another account.

### Description

Sends a single payment in bones to a given account address. Note that 1 HNT it 100_000_000 bones

### Parameters

| Name           | Type    | Constraints | Description                      |
| -------------- | ------- | ----------- | -------------------------------- |
| params         | object  |             |                                  |
| params.address | string  |             | B58 address of the payer wallet  |
| params.payee   | string  |             | B58 address of the payee account |
| params.bones   | integer |             | Amount in bones to send          |

### Result

| Name        | Type   | Constraints | Description                                                                                           |
| ----------- | ------ | ----------- | ----------------------------------------------------------------------------------------------------- |
| result      | object |             | Transaction details. The exact fields returned depend on the transaction type returned in the result. |
| result.hash | string |             | B64 hash of the transaction                                                                           |
| result.type | string |             | The type of the transaction                                                                           |

### Errors

| Code | Message | Description                |
| ---- | ------- | -------------------------- |
| -100 |         | Wallet not found or locked |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_pay",
  "params": {
    "address": "13Ya3s4k8dsbd1dey6dmiYbwk4Dk1MRFCi3RBQ7nwKnSZqnYoW5",
    "payee": "13buBykFQf5VaQtv7mWj2PBY9Lq4i1DeXhg7C4Vbu3ppzqqNkTH",
    "bones": 1000
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {}
}
```

<a name="wallet_pay_multi"></a>

## wallet_pay_multi

Send multiple paymens in a single transation.

### Description

Sends multiple payments in bones to one or more payees. Note that 1 HNT it 100_000_000 bones

### Parameters

| Name                     | Type    | Constraints | Description                      |
| ------------------------ | ------- | ----------- | -------------------------------- |
| params                   | object  |             |                                  |
| params.address           | string  |             | B58 address of the payer wallet  |
| params.payments          | array   |             |                                  |
| params.payments[]        | object  |             |                                  |
| params.payments[]?.payee | string  |             | B58 address of the payee account |
| params.payments[]?.bones | integer |             | Amount in bones to send          |

### Result

| Name        | Type   | Constraints | Description                                                                                           |
| ----------- | ------ | ----------- | ----------------------------------------------------------------------------------------------------- |
| result      | object |             | Transaction details. The exact fields returned depend on the transaction type returned in the result. |
| result.hash | string |             | B64 hash of the transaction                                                                           |
| result.type | string |             | The type of the transaction                                                                           |

### Errors

| Code | Message | Description                |
| ---- | ------- | -------------------------- |
| -100 |         | Wallet not found or locked |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_pay_multi",
  "params": {
    "payments": [{}]
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {}
}
```

<a name="wallet_import"></a>

## wallet_import

Import an encrypted wallet.

### Description

Import an encrypted wallet into the wallet database. The password is only used to verify that the wallet can be unlocked and is not stored.

### Parameters

| Name            | Type   | Constraints | Description                                |
| --------------- | ------ | ----------- | ------------------------------------------ |
| params          | object |             |                                            |
| params.password | string |             | Password used to decrypt the wallet        |
| params.path     | string |             | Path to the file to import the wallet from |

### Result

| Name   | Type    | Constraints | Description                             |
| ------ | ------- | ----------- | --------------------------------------- |
| result | boolean |             | Returns true if the wallet was imported |

### Errors

| Code | Message | Description                 |
| ---- | ------- | --------------------------- |
| -100 |         | Wallet file not found       |
| -110 |         | Invalid password for wallet |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_import",
  "params": {
    "password": "a password"
  }
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": true
}
```

<a name="wallet_export"></a>

## wallet_export

Export an encrypted wallet to a given path.

### Description

Exports an encrypted wallet to the given path.

### Parameters

| Name           | Type   | Constraints | Description                            |
| -------------- | ------ | ----------- | -------------------------------------- |
| params         | object |             |                                        |
| params.address | string |             | B58 address of the payer wallet        |
| params.path    | string |             | Path to the file to save the wallet to |

### Result

| Name        | Type   | Constraints | Description                                                                                           |
| ----------- | ------ | ----------- | ----------------------------------------------------------------------------------------------------- |
| result      | object |             | Transaction details. The exact fields returned depend on the transaction type returned in the result. |
| result.hash | string |             | B64 hash of the transaction                                                                           |
| result.type | string |             | The type of the transaction                                                                           |

### Errors

| Code | Message | Description      |
| ---- | ------- | ---------------- |
| -100 |         | Wallet not found |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_export",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {}
}
```

<a name="wallet_backup_list"></a>

## wallet_backup_list

Lists information on the list of backups in the given path.

### Description

Backup list information includes the backup ID, size, and the time the backup was created.

### Parameters

| Name        | Type   | Constraints | Description               |
| ----------- | ------ | ----------- | ------------------------- |
| params      | object |             |                           |
| params.path | string |             | Path to the backup folder |

### Result

| Name                  | Type    | Constraints | Description                               |
| --------------------- | ------- | ----------- | ----------------------------------------- |
| result                | array   |             |                                           |
| result[]              | object  |             |                                           |
| result[].backup_id    | integer |             | ID of the backup                          |
| result[].number_files | integer |             | Number of files in the backup             |
| result[].size         | integer |             | Size of backup, in bytes                  |
| result[].timestamp    | integer |             | Timestamp (seconds since epoch) of backup |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_backup_list",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": [
    {
      "backup_id": 2,
      "number_files": 3
    }
  ]
}
```

<a name="wallet_backup_create"></a>

## wallet_backup_create

Creates a backup of the wallet database.

### Description

Creates a backup of the backup database in the given path.

### Parameters

| Name               | Type    | Constraints | Description                                         |
| ------------------ | ------- | ----------- | --------------------------------------------------- |
| params             | object  |             |                                                     |
| params.path        | string  |             | Path to the backup folder                           |
| params.max_backups | integer |             | Maximum number of backups to maintain in the folder |

### Result

| Name                | Type    | Constraints | Description                               |
| ------------------- | ------- | ----------- | ----------------------------------------- |
| result              | object  |             |                                           |
| result.backup_id    | integer |             | ID of the backup                          |
| result.number_files | integer |             | Number of files in the backup             |
| result.size         | integer |             | Size of backup, in bytes                  |
| result.timestamp    | integer |             | Timestamp (seconds since epoch) of backup |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_backup_create",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "result": {
    "backup_id": 2,
    "number_files": 3
  }
}
```

<a name="wallet_backup_delete"></a>

## wallet_backup_delete

Delete a backup.

### Description

Delete the backup with the given ID from the given backup path.

### Parameters

| Name             | Type    | Constraints | Description               |
| ---------------- | ------- | ----------- | ------------------------- |
| params           | object  |             |                           |
| params.path      | string  |             | Path to the backup folder |
| params.backup_id | integer |             | Backup ID to delete       |

### Result

| Name   | Type    | Constraints | Description                                |
| ------ | ------- | ----------- | ------------------------------------------ |
| result | boolean |             | True if the backup was deleted succesfully |

### Errors

| Code | Message | Description      |
| ---- | ------- | ---------------- |
| -100 |         | Backup not found |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_backup_delete",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890"
}
```

<a name="wallet_backup_restore"></a>

## wallet_backup_restore

Restore the wallet database.

### Description

Restores the wallet database from the backup ID in the given backup folder.

### Parameters

| Name             | Type    | Constraints | Description               |
| ---------------- | ------- | ----------- | ------------------------- |
| params           | object  |             |                           |
| params.path      | string  |             | Path to the backup folder |
| params.backup_id | integer |             | Backup ID to restore from |

### Result

| Name   | Type    | Constraints | Description                                 |
| ------ | ------- | ----------- | ------------------------------------------- |
| result | boolean |             | True if the backup was restored succesfully |

### Errors

| Code | Message | Description      |
| ---- | ------- | ---------------- |
| -100 |         | Backup not found |

### Examples

#### Request

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "wallet_backup_restore",
  "params": {}
}
```

#### Response

```json
{
  "jsonrpc": "2.0",
  "id": "1234567890"
}
```
