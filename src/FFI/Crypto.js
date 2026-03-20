// AES-256-GCM at-rest encryption for localStorage.
// Generates a random key on first use, stores it in
// localStorage. Tokens are encrypted before storage
// and decrypted on load.

var te = new TextEncoder();
var td = new TextDecoder();

function toBase64(buf) {
  return btoa(
    String.fromCharCode(...new Uint8Array(buf))
  );
}

function fromBase64(str) {
  return Uint8Array.from(atob(str), (c) =>
    c.charCodeAt(0)
  );
}

async function getLocalKey() {
  var stored = localStorage.getItem("cge-crypto-key");
  if (stored) {
    return crypto.subtle.importKey(
      "jwk",
      JSON.parse(stored),
      { name: "AES-GCM" },
      true,
      ["encrypt", "decrypt"]
    );
  }
  var key = await crypto.subtle.generateKey(
    { name: "AES-GCM", length: 256 },
    true,
    ["encrypt", "decrypt"]
  );
  var jwk = await crypto.subtle.exportKey("jwk", key);
  localStorage.setItem(
    "cge-crypto-key",
    JSON.stringify(jwk)
  );
  return key;
}

async function encryptLocal(plaintext) {
  var key = await getLocalKey();
  var iv = crypto.getRandomValues(new Uint8Array(12));
  var ct = await crypto.subtle.encrypt(
    { name: "AES-GCM", iv },
    key,
    te.encode(plaintext)
  );
  var buf = new Uint8Array(iv.length + ct.byteLength);
  buf.set(iv, 0);
  buf.set(new Uint8Array(ct), iv.length);
  return toBase64(buf);
}

async function decryptLocal(blob) {
  var key = await getLocalKey();
  var data = fromBase64(blob);
  var iv = data.slice(0, 12);
  var ct = data.slice(12);
  var pt = await crypto.subtle.decrypt(
    { name: "AES-GCM", iv },
    key,
    ct
  );
  return td.decode(pt);
}

export const encryptToken = (token) => () =>
  encryptLocal(token);

export const decryptToken = (blob) => () =>
  decryptLocal(blob).catch(() => {
    // Migration: plaintext token from before
    // encryption. Re-encrypt and return original.
    return encryptLocal(blob).then((enc) => {
      localStorage.setItem("cge-token", enc);
      return blob;
    });
  });
