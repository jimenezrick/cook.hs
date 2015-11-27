import Cook.Receipt

main = runPlan $ do
    proc "ssh" ["buyvm.untroubled.be"]
