names <- c("coye", "paul", "andrew", "judd")

y <- c(9, 7, 6, 6)

data.frame(chef = names, score = y)

data.frame(chef = names, score = y) -> pr

pr

pr$chef

names(pr)

names(pr)[2] = "score1"
pr

pr$score2 <- c(3, 2, 1, 2)

pr$spiciness <- c(3, 2, 1, 2)

pr

attach(pr)

score1

score1 = score1 + 10

score1
pr$score1


mean(pr$score1)

pr$score1 <- scale(pr$score1)

pr$score1

pr$score2 = scale(pr$score2)

pr

pr$total_Score <- (pr$score1 + pr$score2) / 2

pr

pr$above_av <- pr$total_Score > mean(pr$total_Score)

pr

pr$spiciness

pr$spiciness <- factor(pr$spiciness, levels = c(1, 2, 3), labels = c("mild", "spicy", "extra spicy"))

pr

levels(pr$spiciness)

levels(pr$spicines) <- c("mild", "medium", "hot")

pr

