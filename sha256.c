#define NULL 0
#define SHA256_DIGEST_LENGTH 32
typedef __SIZE_TYPE__ size_t;
typedef unsigned char __uint8_t;
typedef __uint8_t uint8_t;

void *evp_generic_fetch(OSSL_LIB_CTX *libctx, int operation_id,
                        const char *name, const char *properties,
                        void *(*new_method)(int name_id,
                                            const OSSL_ALGORITHM *algodef,
                                            OSSL_PROVIDER *prov),
                        int (*up_ref_method)(void *),
                        void (*free_method)(void *))
{
    struct evp_method_data_st methdata;
    void *method;

    methdata.libctx = libctx;
    methdata.tmp_store = NULL;
    method = inner_evp_generic_fetch(&methdata, NULL, operation_id,
                                     name, properties,
                                     new_method, up_ref_method, free_method);
    dealloc_tmp_evp_method_store(methdata.tmp_store);
    return method;
}

int EVP_Digest(const void *data, size_t count,
               unsigned char *md, unsigned int *size, const void *type,
               void *impl)
{
    void *ctx = EVP_MD_CTX_new();
    int ret;

    if (ctx == NULL)
        return 0;
    EVP_MD_CTX_set_flags(ctx, EVP_MD_CTX_FLAG_ONESHOT);
    ret = EVP_DigestInit_ex(ctx, type, impl)
        && EVP_DigestUpdate(ctx, data, count)
        && EVP_DigestFinal_ex(ctx, md, size);
    EVP_MD_CTX_free(ctx);

    return ret;
}

void *EVP_MD_fetch(void *ctx, const char *algorithm,
                     const char *properties)
{
    void *md =
        evp_generic_fetch(ctx, OSSL_OP_DIGEST, algorithm, properties,
                          evp_md_from_algorithm, evp_md_up_ref, evp_md_free);

    return md;
}


int EVP_Q_digest(void *libctx, const char *name, const char *propq,
                 const void *data, size_t datalen,
                 unsigned char *md, size_t *mdlen)
{
    void *digest = EVP_MD_fetch(libctx, name, propq);
    unsigned int temp = 0;
    int ret = 0;

    if (digest != NULL) {
        ret = EVP_Digest(data, datalen, md, &temp, digest, NULL);
        EVP_MD_free(digest);
    }
    if (mdlen != NULL)
        *mdlen = temp;
    return ret;
}

unsigned char *SHA256(const unsigned char *d, size_t n, unsigned char *md)
{
    static unsigned char m[SHA256_DIGEST_LENGTH];

    if (md == NULL)
        md = m;
    return EVP_Q_digest(NULL, "SHA256", NULL, d, n, md, NULL) ? md : NULL;
}

void
urcrypt_shay(const uint8_t *message, size_t length, uint8_t out[32])
{
  SHA256(message, length, out);
}

