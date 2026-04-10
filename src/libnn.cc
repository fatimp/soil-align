#include <faiss/utils/distances.h>

using idx_t = int64_t;

extern "C" {
void knn_search (const float *qs, const float *ps, size_t d, size_t nqs, size_t nps,
                 float *dists, idx_t *indices, size_t k);
}

void knn_search (const float *qs, const float *ps, size_t d, size_t nqs, size_t nps,
                 float *dists, idx_t *indices, size_t k) {
    faiss::float_maxheap_array_t res = { nqs, k, indices, dists };
    faiss::knn_L2sqr (qs, ps, d, nqs, nps, &res, nullptr, nullptr);
}
