module ParseSpec where

import           Test.Hspec

import           Parse

spec :: Spec
spec = do
  describe "parseVerse" $ do
    it "parses a verse" $ do
      let raw = "{4:13} But rejoice, inasmuch as ye are partakers of Christ's sufferings ..."
      parseVerse raw `shouldBe` Right (Verse Nothing "4" "13" "But rejoice, inasmuch as ye are partakers of Christ's sufferings ...", "")

    it "ignores trailing whitespace" $ do
      let raw = "   {4:13} But rejoice, inasmuch as ye are partakers of Christ's sufferings ..."
      parseVerse raw `shouldBe` Right (Verse Nothing "4" "13" "But rejoice, inasmuch as ye are partakers of Christ's sufferings ...", "")

    it "parses blocks" $ do
      let raw = "{13:8} For we can do nothing against the truth, but for the truth. {13:9} For we are glad, when we are weak, and ye are strong: and this also we wish, [even] your perfection. {13:10} Therefore I write these things being absent, lest being present I should use sharpness, according to the power which the Lord hath given me to edification, and not to destruction."
      parseVerse raw
        `shouldBe`
          Right (
            Verse
              Nothing
              "13"
              "8"
              "For we can do nothing against the truth, but for the truth.",
              "{13:9} For we are glad, when we are weak, and ye are strong: and this also we wish, [even] your perfection. {13:10} Therefore I write these things being absent, lest being present I should use sharpness, according to the power which the Lord hath given me to edification, and not to destruction."
              )

    it "ignores newlines" $ do
      let raw = "{1:14} As also ye have acknowledged us in part, that we are\n your rejoicing, even as ye also [are] ours in the day of the Lord Jesus.\n \n {1:15} And in this confidence I was minded to come unto you before,\n that ye might have a second benefit;"
      parseVerse raw
        `shouldBe`
          Right (
            Verse
              Nothing
              "1"
              "14"
              "As also ye have acknowledged us in part, that we are your rejoicing, even as ye also [are] ours in the day of the Lord Jesus.",
              "{1:15} And in this confidence I was minded to come unto you before,\n that ye might have a second benefit;"
              )

    it "identifies book name" $ do
      let raw = "\n\tThe First Epistle of Paul the Apostle to Timothy\n\n {1:1} Paul, an apostle of Jesus Christ by the commandment of God our Saviour, and Lord Jesus Christ, [which is] our hope;"
      parseVerse raw
        `shouldBe`
          Right (
            Verse
              (Just "The First Epistle of Paul the Apostle to Timothy")
              "1"
              "1"
              "Paul, an apostle of Jesus Christ by the commandment of God our Saviour, and Lord Jesus Christ, [which is] our hope;",
              "")
