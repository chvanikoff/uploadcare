# Uploadcare client for Erlang

## Usage
1. Get your public and private keys at https://uploadcare.com/accounts/settings/

```erlang
Auth_data = [{public_key, "your_pub_key"}, {private_key, "your_priv_key"}].
```

2. Start uploadcare app (following examples are equal):

```erlang
uploadcare:start(Auth_data).
```

```erlang
uploadcare:start(),
uploadcare:set_auth_data(Auth_data).
```

```erlang
application:start(uploadcare),
uploadcare:set_auth_data(Auth_data).
```

3. Use the library to get, store or delete files from/to Uploadcare.com

```erlang
UUID = "some-uuid-that-you've-got-from-post-data-for-example",
File_data = (uploadcare:info(UUID))(), %% Just get a JSON data for the file
File_data2 = (uploadcare:store(UUID))(), %% Store the file
File_data3 = (uploadcare:delete(UUID))(), %% Delete the file
```
