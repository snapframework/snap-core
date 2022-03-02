## 1.0.5.0

- Add `head` helper for `Snap.Test`
- Add support for GHC 9
- Expose Snap.Internal.Util.FileServe
- Ensure parseHttpTime returns 0 on invalid input
- Add test helper for HEAD requests
- Misc version bumps

## 1.0.4.0
- Allow `network` 3.0.

## 1.0.3.2
- Allow `unix-compat` 0.5

- Stop using deprecated Data.Map.insertWith'

- Test with GHC 8.4.1

## 1.0.3.1
- Allow `io-streams` 1.5.

## 1.0.3.0
### Added
- Alternative file upload handling into Snap.Util.FileUploads

### Fixes
- Fixed parsing of field values in multipart/form-data headers with encodings
  other than US-ASCII

## 1.0.2.1
### Dependencies
- Allow `io-streams` 1.4.

## 1.0.2.0
### Added
- Merged CORS functionality from snap-cors project into Snap.Util.CORS

### Dependencies
- Bumped `time` dependency.

## 1.0.1.1
### Fixes
- Fixed a bug in token parsing in Snap.Util.Parsing

## 1.0.1.0
### Fixes
- fixed a FileUpload exception test

- Exported `compressibleMimeTypes`

- Added a missing file to the tarball.

## 1.0.0.0
### Added
### Removed

 - Removed support for `iteratees` in favor of
   [io-streams](https://hackage.haskell.org/package/io-streams)
