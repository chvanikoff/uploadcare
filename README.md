# Uploadcare client for Erlang

## Usage
- Get your public and private keys at https://uploadcare.com/accounts/settings/

```erlang
Auth_data = [{public_key, "your_pub_key"}, {private_key, "your_priv_key"}].
```

- Start uploadcare app (following examples are equal):

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

- Use the library to upload, store, get or delete files from/to Uploadcare.com

```erlang
UUID = "some-uuid-that-you've-got-from-post-data-for-example",
File_data = uploadcare:info(UUID), %% Just get a JSON data for the file
File_data2 = uploadcare:store(UUID), %% Store the file
File_data3 = uploadcare:delete(UUID), %% Delete the file
```

- File uploading could be implemented in 2 steps:
```erlang
%% Send upload request and get token
Token = uploadcare:upload_url("http://learnyousomeerlang.com/static/img/erlang-the-movie.png"),
%% And check upload status for the token
Status = uploadcare:upload_status(Token), %% Result is a JSON where UUID of uploaded file could be found
```