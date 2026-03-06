// Latin1 encoding: each char code maps directly to a byte value
export const ofStringImpl = (s) => {
  const result = new Array(s.length);
  for (let i = 0; i < s.length; i++) {
    result[i] = s.charCodeAt(i) & 0xFF;
  }
  return result;
};

export const makeImpl = (n) => (c) => {
  const result = new Array(n);
  for (let i = 0; i < n; i++) {
    result[i] = c & 0xFF;
  }
  return result;
};

export const arrayLength = (a) => a.length;

export const appendArrays = (a) => (b) => a.concat(b);

export const sliceArray = (start) => (end) => (a) => a.slice(start, end);

export const unsafeIndex = (a) => (i) => a[i];

// Read a signed little-endian 64-bit integer as a string (for BigInt parsing)
export const getInt64LeImpl = (a) => (offset) => {
  let result = BigInt(0);
  for (let i = 7; i >= 0; i--) {
    result = (result << BigInt(8)) | BigInt(a[offset + i]);
  }
  // Interpret as signed 64-bit
  if (result >= BigInt("9223372036854775808")) {
    result -= BigInt("18446744073709551616");
  }
  return result.toString();
};
