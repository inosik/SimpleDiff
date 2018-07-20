module SimpleDiff

type Diff<'a> =
    | Equal of 'a list
    | Added of 'a list
    | Removed of 'a list

type private SubsequenceInfo =
    { /// Count of items in the old list before the common sequence.
      OldPrefixLength : int
      /// Count of items in the new list before the common sequence.
      NewPrefixLength : int
      /// The length of the longest common subsequence.
      CommonSeqLength : int }

module List =
    open System.Collections.Generic

    /// <summary>Find the differences between two lists.</summary>
    /// <param name="projection">A function that returns the value which is used to compare two items.</param>
    /// <param name="before">The old list of values.</param>
    /// <param name="after">The new list of values.</param>
    /// <returns>A list of differences.</returns>
    let diffBy (projection : _ -> 'key) before after =
        // Creates a map from values to their indices
        let index items =
            let indexMap = Dictionary<_, _> ()
            let mutable i = 0
            for value in items do
                let key = projection value
                let indices =
                    match indexMap.TryGetValue key with
                    | true, indices -> indices
                    | _ ->
                        let indices = ResizeArray<_> ()
                        indexMap.[key] <- indices
                        indices
                indices.Add i
                i <- i + 1
            indexMap

        // Find the largest substring common to `old` and `nu`. We use a dynamic
        // programming approach here.
        //
        // `nu` because `new` is a keyword
        let getLongestSubsequence old nu =
            let oldIndexMap = index old

            // `subStartOld` is the index of the beginning og the largest
            // overlapping substring in the old list.
            //
            // `subStartNew` is the index of the beginning of the same substring
            // in the new list.
            //
            // `commonLen` is the length that overlaps in both.
            //
            // These track the largest overlapping substring seen so far, so
            // natrually we start with a 0-length substring.
            let mutable oldPrefixLen = 0
            let mutable newPrefixLen = 0
            let mutable commonLen = 0

            // We iterate over each value in the new list, calling the index
            // `inew`. At each iteration, `overlap[i]` is the length of the
            // largest suffix of `old[0..i - 1]` equal to a suffix of
            // `nu[0..inew - 1`] (or unset when `old[i] != `nu[inew]`).
            //
            // At each stage of iteration, the new `overlap` (called
            // `tmpOverlap` until the original `overlap` is no longer needed) is
            // built from the old one.
            //
            // If the length of overlap exceeds the largest substring seen so
            // far (`commonLen`), we update the largest substring to the
            // overlapping strings.
            let mutable inew = 0
            let mutable overlap = Dictionary<_, _> ()
            for value in nu do
                let tmpOverlap = Dictionary<_, _> ()

                let key = projection value
                match oldIndexMap.TryGetValue key with
                | true, indices ->
                    for iold in indices do
                        let prevCommonLen =
                            match overlap.TryGetValue (iold - 1) with
                            | true, n -> n
                            | _ -> 0
                        // Now we are considering all values of `iold` such that
                        // `old[iold] == nu[inew]`.
                        let newCommonLen = prevCommonLen + 1
                        tmpOverlap.[iold] <- newCommonLen

                        if newCommonLen > commonLen then
                            // This is the largest substring seen so far, so
                            // store its indices
                            commonLen <- newCommonLen
                            oldPrefixLen <- iold - newCommonLen + 1
                            newPrefixLen <- inew - newCommonLen + 1
                | _ -> ()

                overlap <- tmpOverlap
                inew <- inew + 1

            { OldPrefixLength = oldPrefixLen
              NewPrefixLength = newPrefixLen
              CommonSeqLength = commonLen }

        let rec diff old nu =
            match old, nu with
            | [], [] -> []
            | old, [] -> [Removed old]
            | [], nu -> [Added nu]
            | old, nu ->
                match getLongestSubsequence old nu with
                | { CommonSeqLength = 0 } ->
                    [ Removed old
                      Added nu ]
                | { OldPrefixLength = oldPrefixLen
                    NewPrefixLength = newPrefixLen
                    CommonSeqLength = len } ->
                    let oldPrefix, unchanged, oldSuffix =
                        let prefix, tail = List.splitAt oldPrefixLen old
                        let unchanged, suffix = List.splitAt len tail
                        (prefix, unchanged, suffix)

                    let newPrefix, newSuffix =
                        let prefix, tail = List.splitAt newPrefixLen nu
                        let suffix = tail |> List.splitAt len |> snd
                        (prefix, suffix)

                    [ yield! diff oldPrefix newPrefix
                      yield Equal unchanged
                      yield! diff oldSuffix newSuffix ]
        diff before after

    /// <summary>Find the differences between two lists.</summary>
    /// <param name="before">The old list of values.</param>
    /// <param name="after">The new list of values.</param>
    /// <returns>A list of differences.</returns>
    let diff before after =
        diffBy id before after
