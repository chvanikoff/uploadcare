# Uploadcare client for Erlang

## Usage
1. Get your public and private keys at https://uploadcare.com/accounts/settings/
	> Auth_data = [{public_key, "your_pub_key"}, {private_key, "your_priv_key"}].

2. Start uploadcare app (following examples are equal):
	1> uploadcare:start(Auth_data).

	1> uploadcare:start(),
	2> uploadcare:set_auth_data(Auth_data).

	1> application:start(uploadcare),
	2> uploadcare:set_auth_data(Auth_data).

3. Use the library to get, store or delete files from/to Uploadcare.com
	1> UUID = "some-uuid-that-you've-got-from-post-data-for-example",
	2> File_data = (uploadcare:info(UUID))(), %% Just get a JSON data for the file
	3> File_data2 = (uploadcare:store(UUID))(), %% Store the file
	4> File_data3 = (uploadcare:delete(UUID))(), %% Delete the file