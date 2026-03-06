export const parseDoubleImpl = (s) => {
  const n = parseFloat(s);
  if (isNaN(n)) return null;
  return n;
};
