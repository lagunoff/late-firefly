function __str_idx(str, idx) {
  return str.charCodeAt(idx) || 0;
}
function _hs_text_memcpy(dst, dst_off, src, src_off, n) {
  for (var i = 0; i < n; i++) {
    dst['v']['w16'][i + dst_off] = src['v']['w16'][i + src_off];
  }
}
