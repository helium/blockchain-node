-module(wallet).

-define(IV_LENGTH, 12).
-define(TAG_LENGTH, 16).
-define(PASSWORD_ITERATIONS, 100000).

-define(BASIC_KEY_V1,   16#0001).
-define(BASIC_KEY_V2,   16#0002).
-define(PWHASH_BKDF2,      16#00).
-define(PWHASH_ARGON2ID13, 16#01).

-define(PBKDF2_SALT_LENGTH, 8).
-record(pwhash_pbkdf2,
        {
         salt=undefined :: undefined | <<_:(8 * ?PBKDF2_SALT_LENGTH)>>,
         iterations :: pos_integer()
        }).



-define(ARGON2ID13_SALT_LENGTH, 16).
-record(pwhash_argon2id13,
        {
         salt=undefined :: undefined | <<_:(8 * ?ARGON2ID13_SALT_LENGTH)>>,
         ops_limit :: pwhash_limit(),
         mem_limit :: pwhash_limit()
        }).

-type pwhash() :: #pwhash_pbkdf2{} | #pwhash_argon2id13{}.
-type pwhash_limit() :: interactive | moderate | sensitive | pos_integer().
-type iv() :: <<_:(8 * ?IV_LENGTH)>>.
-type tag() :: <<_:(8 * ?TAG_LENGTH)>>.

-record(wallet,
        {
         pwhash :: pwhash(),
         pubkey_bin :: libp2p_crypto:pubkey_bin(),
         iv :: iv(),
         tag :: tag(),
         encrypted :: binary()
        }).

-type wallet() :: #wallet{}.

-export_type([wallet/0]).

-export([encrypt/2, encrypt/3, decrypt/2, pubkey_bin/1]).
-export([from_binary/1, to_binary/1, to_binary/2]).

-spec encrypt(KeyMap::libp2p_crypto:key_map(), Password::binary()) -> {ok, wallet()} | {error, term()}.
encrypt(KeyMap, Password) ->
    encrypt(KeyMap, Password,
            #pwhash_argon2id13{ops_limit=ops_limit({argon2id13, sensitive}),
                               mem_limit=mem_limit({argon2id13, sensitive})}).

-spec encrypt(KeyMap::libp2p_crypto:key_map(), Password::binary(), PWHash::pwhash())
             -> {ok, wallet()} | {error, term()}.
encrypt(KeyMap, Password, PWHash) ->
    IV = crypto:strong_rand_bytes(?IV_LENGTH),
    {ok, EncryptionKey, NewPWHash} = pwhash(Password, PWHash),
    {PubKeyBin, EncryptBin, Tag} = encrypt_keymap(EncryptionKey, IV, ?TAG_LENGTH, KeyMap),
    {ok, #wallet{
            pubkey_bin=PubKeyBin,
            pwhash=NewPWHash,
            iv=IV,
            tag=Tag,
            encrypted=EncryptBin
           }}.

-spec pubkey_bin(wallet()) -> lib2p_crypto:pubkey_bin().
pubkey_bin(#wallet{pubkey_bin=PubKeyBin}) ->
    PubKeyBin.

-spec decrypt(Password::binary(), wallet()) -> {ok, libp2p_crypto:key_map()} | {error, term()}.
decrypt(Password, #wallet{
                     pubkey_bin=PubKeyBin,
                     iv=IV,
                     tag=Tag,
                     pwhash=PWHash,
                     encrypted=Encrypted
                    }) ->
    {ok, AESKey, PWHash} = pwhash(Password, PWHash),
    decrypt_keymap(AESKey, IV, Tag, PubKeyBin, Encrypted).


-spec from_binary(binary()) -> {ok, wallet()} | {error, term()}.
from_binary(<<(?BASIC_KEY_V1):16/integer-unsigned-little,
              PubKeyBin:33/binary,
              IV:(?IV_LENGTH)/binary,
              Salt:(?PBKDF2_SALT_LENGTH)/binary,
              Iterations:32/integer-unsigned-little,
              Tag:(?TAG_LENGTH)/binary,
              Encrypted/binary>>) ->
    {ok, #wallet{
            pubkey_bin=PubKeyBin,
            iv = IV,
            pwhash = #pwhash_pbkdf2{salt=Salt, iterations=Iterations},
            tag = Tag,
            encrypted = Encrypted
           }};
from_binary(<<(?BASIC_KEY_V2):16/integer-unsigned-little,
              (?PWHASH_BKDF2):8/integer-unsigned,
              PubKeyBin:33/binary,
              IV:(?IV_LENGTH)/binary,
              Salt:(?PBKDF2_SALT_LENGTH)/binary,
              Iterations:32/integer-unsigned-little,
              Tag:(?TAG_LENGTH)/binary,
              Encrypted/binary>>) ->
    {ok, #wallet{
            pubkey_bin=PubKeyBin,
            iv = IV,
            pwhash = #pwhash_pbkdf2{salt=Salt, iterations=Iterations},
            tag = Tag,
            encrypted = Encrypted
           }};
from_binary(<<(?BASIC_KEY_V2):16/integer-unsigned-little,
              (?PWHASH_ARGON2ID13):8/integer-unsigned,
              PubKeyBin:33/binary,
              IV:(?IV_LENGTH)/binary,
              Salt:(?ARGON2ID13_SALT_LENGTH)/binary,
              MemLimit:32/integer-unsigned-little,
              OpsLimit:32/integer-unsigned-little,
              Tag:(?TAG_LENGTH)/binary,
              Encrypted/binary>>) ->
    {ok, #wallet{
            pubkey_bin=PubKeyBin,
            iv = IV,
            pwhash = #pwhash_argon2id13{salt=Salt, ops_limit=OpsLimit, mem_limit=MemLimit},
            tag = Tag,
            encrypted = Encrypted
           }};
from_binary(_) ->
    {error, invalid_wallet}.


-spec to_binary(wallet()) -> binary().
to_binary(Wallet) ->
    to_binary(?BASIC_KEY_V2, Wallet).

-spec to_binary(Version::pos_integer(), wallet() | pwhash()) -> binary().
to_binary(?BASIC_KEY_V1,
          #wallet{ pubkey_bin=PubKeyBin,
                   iv=IV,
                   pwhash=#pwhash_pbkdf2 { salt=Salt, iterations=Iterations },
                   tag=Tag,
                   encrypted=Encrypted}) ->
    <<?BASIC_KEY_V1:16/integer-unsigned-little,
      PubKeyBin:33/binary,
      IV:(?IV_LENGTH)/binary,
      Salt:(?PBKDF2_SALT_LENGTH)/binary,
      Iterations:32/unsigned-integer-little,
      Tag:(?TAG_LENGTH)/binary,
      Encrypted/binary>>;
to_binary(?BASIC_KEY_V1, _) ->
    error(invalid_v1_wallet);
to_binary(?BASIC_KEY_V2,
          #wallet{ pubkey_bin=PubKeyBin,
                   iv=IV,
                   pwhash=PWHash,
                   tag=Tag,
                   encrypted=Encrypted}) ->
    <<?BASIC_KEY_V2:16/integer-unsigned-little,
      (pwhash_kind(PWHash)):8/integer-unsigned,
      PubKeyBin:33/binary,
      IV:(?IV_LENGTH)/binary,
      (to_binary(?BASIC_KEY_V2, PWHash))/binary,
      Tag:(?TAG_LENGTH)/binary,
      Encrypted/binary>>;
to_binary(_, #pwhash_argon2id13{salt=Salt, mem_limit=MemLimit, ops_limit=OpsLimit}) ->
    <<Salt/binary, MemLimit:32/integer-unsigned-little, OpsLimit:32/integer-unsigned-little>>;
to_binary(_, #pwhash_pbkdf2{salt=Salt, iterations=Iterations}) ->
    <<Salt/binary, Iterations:32/integer-unsigned-little>>.

pwhash_kind(#pwhash_pbkdf2{}) ->
    ?PWHASH_BKDF2;
pwhash_kind(#pwhash_argon2id13{}) ->
    ?PWHASH_ARGON2ID13.

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

pwhash(Password, PWHash=#pwhash_pbkdf2{ iterations=Iterations}) ->
    Salt = case PWHash#pwhash_pbkdf2.salt of
               undefined -> crypto:strong_rand_bytes(?PBKDF2_SALT_LENGTH);
               V -> V
           end,
    {ok, AESKey} = pbkdf2:pbkdf2(sha256, Password, Salt, Iterations),
    {ok, AESKey, PWHash#pwhash_pbkdf2{salt=Salt}};
pwhash(Password, PWHash=#pwhash_argon2id13{}) ->
    Salt = case PWHash#pwhash_argon2id13.salt of
               undefined -> crypto:strong_rand_bytes(?ARGON2ID13_SALT_LENGTH);
               V -> V
           end,
    AESKey = enacl:pwhash(Password, Salt, PWHash#pwhash_argon2id13.ops_limit, PWHash#pwhash_argon2id13.mem_limit),
    {ok, AESKey, PWHash#pwhash_argon2id13{salt=Salt}}.


-dialyzer({nowarn_function, ops_limit/1}).      % Not all values are used right now
%% These values come from the libsodium build for constants that are
%% named the same.
-spec ops_limit({argon2id13, pwhash_limit()}) -> pos_integer().
ops_limit({argon2id13, interactive}) ->
    2;
ops_limit({argon2id13, moderate}) ->
    3;
ops_limit({argon2id13, sensitive}) ->
    4;
ops_limit({argon2id13, Num}) when is_integer(Num) ->
    Num.

-dialyzer({nowarn_function, mem_limit/1}).      % Not all values are used right now
%% These values come from the libsodium build for constants that are
%% named the same.
-spec mem_limit({argon2id13, pwhash_limit()}) -> pos_integer().
mem_limit({argon2id13, interactive}) ->
    67108864;
mem_limit({argon2id13, moderate}) ->
    268435456;
mem_limit({argon2id13, sensitive}) ->
    1073741824;
mem_limit({argon2id13, Num}) when is_integer(Num) ->
    Num.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_PASSWORD, <<"password">>).

mk_wallet({argon2id13, _}=PWHash) ->
    KeyMap = libp2p_crypto:generate_keys(ed25519),
    {ok, Wallet} = encrypt(KeyMap, ?TEST_PASSWORD,
                           #pwhash_argon2id13{salt=undefined,
                                              ops_limit=ops_limit(PWHash),
                                              mem_limit=mem_limit(PWHash)}),
    {ok, KeyMap, Wallet};
mk_wallet(argon2id13) ->
    KeyMap = libp2p_crypto:generate_keys(ed25519),
    {ok, Wallet} = encrypt(KeyMap, ?TEST_PASSWORD),
    {ok, KeyMap, Wallet};
mk_wallet(pbkdf2) ->
    KeyMap = libp2p_crypto:generate_keys(ed25519),
    {ok, Wallet} = encrypt(KeyMap, ?TEST_PASSWORD, #pwhash_pbkdf2{iterations=1000}),
    {ok, KeyMap, Wallet}.


roundtrip_default_test() ->
    {ok, KeyMap, Wallet} = mk_wallet(argon2id13),
    {ok, Decrypted} = decrypt(?TEST_PASSWORD, Wallet),
    ?assertEqual(KeyMap, Decrypted).

roundtrip_argon2id13_test() ->
    lists:foreach(fun(Level) ->
                          {ok, KeyMap, Wallet} = mk_wallet({argon2id13, Level}),
                          {ok, Decrypted} = decrypt(?TEST_PASSWORD, Wallet),
                          ?assertEqual(KeyMap, Decrypted)
                  end, [interactive, moderate]).

roundtrip_pbkdf2_test() ->
    {ok, KeyMap, Wallet} = mk_wallet(pbkdf2),
    {ok, Decrypted} = decrypt(?TEST_PASSWORD, Wallet),
    ?assertEqual(KeyMap, Decrypted).

roundtrip_binary_test() ->
    lists:foreach(fun(PWHash) ->
                          {ok, _, Wallet} = mk_wallet(PWHash),
                          WalletBin = wallet:to_binary(Wallet),
                          {ok, Decoded} = wallet:from_binary(WalletBin),
                          ?assertEqual(Wallet, Decoded)
                  end, [{argon2id13, interactive}, pbkdf2]).

roundtrip_binary_v1_test() ->
    {ok, _, Wallet} = mk_wallet(pbkdf2),
    WalletBin = to_binary(?BASIC_KEY_V1, Wallet),
    {ok, Decoded} = wallet:from_binary(WalletBin),
    ?assertEqual(Wallet, Decoded),

    {ok, _, Wallet2} = mk_wallet({argon2id13, interactive}),
    ?assertError(invalid_v1_wallet, to_binary(?BASIC_KEY_V1, Wallet2)).

invalid_binary_test() ->
    ?assertEqual({error, invalid_wallet},
                 wallet:from_binary(<<"invalid wallet binary">>)).

roundtrip_pbkdf2_binary_test() ->
    {ok, _, Wallet} = mk_wallet(pbkdf2),
    WalletBin = wallet:to_binary(Wallet),
    {ok, Decoded} = wallet:from_binary(WalletBin),
    ?assertEqual(Wallet, Decoded).

invalid_password_test() ->
    {ok, _, Wallet} = mk_wallet({argon2id13,interactive}),
    ?assertEqual({error, decrypt}, wallet:decrypt(<<"invalid_password">>, Wallet)).

pubkey_bin_test() ->
    {ok, KeyMap, Wallet} = mk_wallet({argon2id13, interactive}),
    #{ public := PubKey } = KeyMap,
    ?assertEqual(libp2p_crypto:pubkey_to_bin(PubKey),
                wallet:pubkey_bin(Wallet)).

-endif.
