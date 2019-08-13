export function invariant(bool, msg = 'invariant failed') {
  if (!bool) {
    throw new Error(msg)
  }
}
