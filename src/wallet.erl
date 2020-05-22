-module(wallet).

-define(IV_LENGTH, 12).
-define(TAG_LENGTH, 16).
-define(SALT_LENGTH, 8).
-define(PASSWORD_ITERATIONS, 100000).

-define(BASIC_KEY_V2,   16#0001).

-type iv() :: <<_:(8 * ?IV_LENGTH)>>.
-type tag() :: <<_:(8 * ?TAG_LENGTH)>>.
-type salt() :: <<_:(8 * ?SALT_LENGTH)>>.

-record(wallet, {
                  pubkey_bin :: libp2p_crypto:pubkey_bin(),
                  iterations :: pos_integer(),
                  iv :: iv(),
                  tag :: tag(),
                  salt :: salt(),
                  encrypted :: binary()
                 }).

-type wallet() :: #wallet{}.

-export_type([wallet/0]).

-export([new/1, pubkey_bin/1, decrypt/2]).
-export([from_binary/1, to_binary/1]).

-spec new(Password::binary()) -> {ok, wallet()} | {error, term()}.
new(Password) ->
    KeyMap = libp2p_crypto:generate_keys(ed25519),
    IV = crypto:strong_rand_bytes(?IV_LENGTH),
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    {ok, AESKey} = pbkdf2:pbkdf2(sha256, Password, Salt, ?PASSWORD_ITERATIONS),
    {PubKeyBin, EncryptBin, Tag} = encrypt_keymap(AESKey, IV, ?TAG_LENGTH, KeyMap),
    {ok, #wallet{
            pubkey_bin=PubKeyBin,
            iterations=?PASSWORD_ITERATIONS,
            iv=IV,
            salt=Salt,
            tag=Tag,
            encrypted=EncryptBin
           }}.

pubkey_bin(#wallet{pubkey_bin=PubKeyBin}) ->
    PubKeyBin.


-spec decrypt(Password::binary(), wallet()) -> {ok, libp2p_crypto:key_map()} | {error, term()}.
decrypt(Password, #wallet{
                     pubkey_bin=PubKeyBin,
                     iv=IV,
                     tag=Tag,
                     salt=Salt,
                     iterations=Iterations,
                     encrypted=Encrypted
                    }) ->
    {ok, AESKey} = pbkdf2:pbkdf2(sha256, Password, Salt, Iterations),
    decrypt_keymap(AESKey, IV, Tag, PubKeyBin, Encrypted).


-spec from_binary(binary()) -> {ok, wallet()} | {error, term()}.
from_binary(<<Version:16/integer-unsigned-little, Payload/binary>>)
  when Version == ?BASIC_KEY_V2 ->
    <<PubKeyBin:33/binary,
      IV:(?IV_LENGTH)/binary,
      Salt:8/binary,
      Iterations:32/integer-unsigned-little,
      Tag:(?TAG_LENGTH)/binary,
      Encrypted/binary>> = Payload,
    {ok, #wallet{
            pubkey_bin=PubKeyBin,
            iv = IV,
            salt = Salt,
            iterations = Iterations,
            tag = Tag,
            encrypted = Encrypted
           }}.

-spec to_binary(wallet()) -> {ok, binary()} | {error, term()}.
to_binary(#wallet{ pubkey_bin=PubKeyBin,
                    iv=IV,
                    salt=Salt,
                    tag=Tag,
                    iterations=Iterations,
                    encrypted=Encrypted}) ->
    {ok, <<?BASIC_KEY_V2:16/integer-unsigned-little,
           PubKeyBin:33/binary,
           IV:(?IV_LENGTH)/binary,
           Salt:8/binary,
           Iterations:32/integer-unsigned-little,
           Tag:(?TAG_LENGTH)/binary,
           Encrypted/binary>>}.

-spec encrypt_keymap(Key::binary(), IV::binary(), TagLength::pos_integer(), KeyMap::libp2p_crypto:key_map())
                    -> {PubKeyBin::binary(), Encrypted::binary(), Tag::binary()}.
encrypt_keymap(Key, IV, TagLength, KeyMap=#{public := PubKey}) ->
    KeysBin = libp2p_crypto:keys_to_bin(KeyMap),
    PubKeyBin = libp2p_crypto:pubkey_to_bin(PubKey),
    {Encrypted, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, KeysBin, PubKeyBin, TagLength, true),
    {PubKeyBin, Encrypted, Tag}.

-spec decrypt_keymap(Key::binary(), IV::binary(), Tag::binary(), PubKeyBin::libp2p_crypto:pubkey_bin(),
                     Encryted::binary()) -> {ok, libp2p_crypto:key_map()} | {error, term()}.
decrypt_keymap(Key, IV, Tag, PubKeyBin, Encrypted) ->
    case crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Encrypted, PubKeyBin, Tag, false) of
        error ->
            {error, decrypt};
        Bin ->
            {ok, libp2p_crypto:keys_from_bin(Bin)}
    end.
