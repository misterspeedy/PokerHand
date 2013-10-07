module PokerHandTests

open Poker.PokerHand
open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``Given various hands``() =

    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "Royal Flush")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "Straight Flush")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "Four of a Kind")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "Full House")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Flush")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "Straight")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "Three of a Kind")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "Two Pair")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "One Pair")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "High Card")>]
    member t.``the hand is correctly identified``(cs : string, expected: string) =
        let hand = Hand.FromString(cs)
        let actual = HandTypeName hand
        actual |> should equal expected

    // Royal Flush beats everything:
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "Q♣ J♣ 10♣ 9♣ 8♣", "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "9♣ 9♠ 9♦ 9♥ J♥",  "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "3♣ 3♠ 3♦ 6♣ 6♥",  "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "Q♣ 10♣ 7♣ 6♣ 4♣", "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "Q♣ J♠ 10♠ 9♥ 8♥", "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "2♦ 2♠ 2♣ K♠ 6♥",  "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Everything is beaten by Royal Flush:
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥",  "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥",  "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥",  "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥",  "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠",  "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]

    // Straight Flush beats the right hands:
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "9♣ 9♠ 9♦ 9♥ J♥",  "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "3♣ 3♠ 3♦ 6♣ 6♥",  "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "Q♣ 10♣ 7♣ 6♣ 4♣", "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "Q♣ J♠ 10♠ 9♥ 8♥", "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "2♦ 2♠ 2♣ K♠ 6♥",  "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Straight Flush loses to the right hands:
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "A♦ K♦ Q♦ J♦ 10♦",  "Lose")>]

    // Straight Flush beats the right other straight flush hands:
    [<TestCase("Q♣ J♣ 10♣ 9♣ 8♣", "J♦ 10♦ 9♦ 8♦ 7♦",  "Win")>]

    // Straight Flush loses to the right other straight flush hands:
    [<TestCase("10♣ 9♣ 8♣ 7♣ 6♣", "J♦ 10♦ 9♦ 8♦ 7♦",  "Lose")>]

    // Four of a Kind beats the right hands:
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "3♣ 3♠ 3♦ 6♣ 6♥",  "Win")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Win")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "Q♣ J♠ 10♠ 9♥ 8♥", "Win")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "2♦ 2♠ 2♣ K♠ 6♥",  "Win")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Four of a Kind loses to the right hands:
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]

    // Four of a Kind beats the right other four of a kind hands:
    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "8♣ 8♠ 8♦ 8♥ J♥",  "Win")>]

    // Four of a Kind loses to the right other four of a kind hands:
    [<TestCase("8♣ 8♠ 8♦ 8♥ J♥", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]

    // Full House beats the right hands:
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Win")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "Q♣ J♠ 10♠ 9♥ 8♥", "Win")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "2♦ 2♠ 2♣ K♠ 6♥",  "Win")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Full House loses to the right hands:
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]

    // Full House beats the right other full house hands:
    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "2♥ 2♠ 2♦ 6♠ 6♦",  "Win")>]

    // Full House loses to the right other full house hands:
    [<TestCase("2♥ 2♠ 2♦ 6♠ 6♦", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]

    // Flush beats the right hands:
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♣ J♠ 10♠ 9♥ 8♥", "Win")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "2♦ 2♠ 2♣ K♠ 6♥",  "Win")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Flush loses to the right hands:
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]

    // Flush beats the right other flush hands:
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "10♦ 7♦ 6♦ 4♦ 2♦", "Win")>] // Difference at first card
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 7♦ 6♦ 4♦ 2♦",  "Win")>] // Difference at second card
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 6♦ 4♦ 2♦", "Win")>] // Difference at third card
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 4♦ 2♦", "Win")>] // Difference at fourth card
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 6♦ 2♦", "Win")>] // Difference at fifth card

    // Flush loses to the right other flush hands:
    [<TestCase("10♦ 7♦ 6♦ 4♦ 2♦", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>] // Difference at first card
    [<TestCase("Q♦ 7♦ 6♦ 4♦ 2♦",  "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>] // Difference at second card
    [<TestCase("Q♦ 10♦ 6♦ 4♦ 2♦", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>] // Difference at third card
    [<TestCase("Q♦ 10♦ 7♦ 4♦ 2♦", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>] // Difference at fourth card
    [<TestCase("Q♦ 10♦ 7♦ 6♦ 2♦", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>] // Difference at fifth card

    // Flush draws with the right other flush hands:
    [<TestCase("Q♦ 10♦ 7♦ 6♦ 2♦", "Q♣ 10♣ 7♣ 6♣ 2♣", "Draw")>]

    // Straight beats the right hands:
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "2♦ 2♠ 2♣ K♠ 6♥",  "Win")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Straight loses to the right hands:
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>]

    // Straight beats the right other straight hands:
    [<TestCase("Q♣ J♠ 10♠ 9♥ 8♥", "J♠ 10♠ 9♥ 8♥ 7♦", "Win")>]
    [<TestCase("A♦ K♥ Q♠ J♦ 10♥", "K♠ Q♦ J♣ 10♠ 9♣", "Win")>] // Ace-high case
    [<TestCase("6♦ 5♥ 4♠ 3♦ 2♥", "5♦ 4♥ 3♠ 2♦ A♥", "Win")>]   // Ace-low case
    [<TestCase("A♦ K♥ Q♠ J♦ 10♥", "5♦ 4♥ 3♠ 2♦ A♥", "Win")>]  // An ace at either end

    // Straight loses to the right other straight hands:
    [<TestCase("J♠ 10♠ 9♥ 8♥ 7♦", "Q♣ J♠ 10♠ 9♥ 8♥", "Lose")>]
    [<TestCase("K♠ Q♦ J♣ 10♠ 9♣", "A♦ K♥ Q♠ J♦ 10♥", "Lose")>] // Ace-high case
    [<TestCase("5♦ 4♥ 3♠ 2♦ A♥", "6♦ 5♥ 4♠ 3♦ 2♥",   "Lose")>] // Ace-low case
    [<TestCase("5♦ 4♥ 3♠ 2♦ A♥", "A♦ K♥ Q♠ J♦ 10♥",  "Lose")>] // An ace at either end

    // Straight draws with the right other straight hands:
    [<TestCase("J♠ 10♠ 9♥ 8♥ 7♦", "J♦ 10♥ 9♣ 8♦ 7♥", "Draw")>]
    [<TestCase("A♦ K♥ Q♠ J♦ 10♥", "A♥ K♦ Q♥ J♣ 10♠", "Draw")>] // Ace-high case
    [<TestCase("5♦ 4♥ 3♠ 2♦ A♥", "5♣ 4♦ 3♥ 2♣ A♠", "Draw")>]   // Ace-low case

    // Three of a Kind beats the right hands:
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "J♥ J♣ 4♣ 4♠ 9♥",  "Win")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Three of a Kind loses to the right hands:
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>]
    [<TestCase("2♦ 2♠ 2♣ K♠ 6♥", "Q♣ J♠ 10♠ 9♥ 8♥", "Lose")>]

    // Two Pair beats the right hands:
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // Two Pair loses to the right hands:
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "Q♣ J♠ 10♠ 9♥ 8♥", "Lose")>]
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "2♦ 2♠ 2♣ K♠ 6♥",  "Lose")>]

    // Two Pair beats the right other two pair hands:
    [<TestCase("K♥ K♣ 4♦ 4♥ 9♣", "J♥ J♣ 4♣ 4♠ 10♥", "Win")>] // Difference on highest pair
    [<TestCase("K♥ K♣ 5♦ 5♥ 9♣", "K♦ K♠ 4♣ 4♠ 10♥", "Win")>] // Difference on second highest pair
    [<TestCase("K♥ K♣ 5♦ 5♥ 10♣", "K♦ K♠ 5♣ 5♠ 9♥", "Win")>] // Difference on highest kicker

    // Two Pair loses to the right other two pair hands:
    [<TestCase("J♥ J♣ 4♣ 4♠ 10♥", "K♥ K♣ 4♦ 4♥ 9♣",  "Lose")>] // Difference on highest pair
    [<TestCase("K♦ K♠ 4♣ 4♠ 10♥", "K♥ K♣ 5♦ 5♥ 9♣",  "Lose")>] // Difference on second highest pair
    [<TestCase("K♦ K♠ 5♣ 5♠ 9♥",  "K♥ K♣ 5♦ 5♥ 10♣", "Lose")>] // Difference on highest kicker

    // Two Pair draws with the right other two pair hands:
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "J♦ J♠ 4♦ 4♥ 9♦", "Draw")>]

    // One Pair beats the right hands:
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "K♥ J♥ 8♣ 7♦ 4♠",  "Win")>]

    // One Pair loses to the right hands:
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "Q♣ J♠ 10♠ 9♥ 8♥", "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "2♦ 2♠ 2♣ K♠ 6♥", "Lose")>]
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "J♥ J♣ 4♣ 4♠ 9♥", "Lose")>]

    // One Pair beats the right other one pair hands:
    [<TestCase("Q♥ Q♠ K♠ 10♦ 5♠", "4♥ 4♠ K♠ 10♦ 5♠", "Win")>] // Difference on the pair
    [<TestCase("Q♥ Q♠ K♠ J♦ 5♠", "Q♦ Q♣ K♠ 10♦ 5♠", "Win")>]  // Difference on the first kicker
    [<TestCase("Q♥ Q♠ K♠ J♦ 5♠", "Q♦ Q♣ K♦ 10♦ 5♠", "Win")>]  // Difference on the second kicker
    [<TestCase("Q♥ Q♠ K♠ J♦ 5♠", "Q♦ Q♣ K♦ J♠ 4♠", "Win")>]   // Difference on the third kicker

    // One Pair loses to the right other one pair hands:
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "Q♥ Q♠ K♠ 10♦ 5♠", "Lose")>] // Difference on the pair
    [<TestCase("Q♦ Q♣ K♠ 10♦ 5♠","Q♥ Q♠ K♠ J♦ 5♠",   "Lose")>] // Difference on the first kicker
    [<TestCase("Q♦ Q♣ K♦ 10♦ 5♠","Q♥ Q♠ K♠ J♦ 5♠",   "Lose")>] // Difference on the second kicker
    [<TestCase("Q♦ Q♣ K♦ J♠ 4♠", "Q♥ Q♠ K♠ J♦ 5♠",   "Lose")>] // Difference on the third kicker

    // One Pair draws with the right other one pair hands:
    [<TestCase("4♥ 4♠ K♠ 10♦ 5♠", "4♦ 4♣ K♦ 10♠ 5♥", "Draw")>]

    // High card loses to the right hands:
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "A♦ K♦ Q♦ J♦ 10♦", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "Q♣ J♣ 10♣ 9♣ 8♣", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "9♣ 9♠ 9♦ 9♥ J♥",  "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "3♣ 3♠ 3♦ 6♣ 6♥",  "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "Q♣ J♠ 10♠ 9♥ 8♥", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "2♦ 2♠ 2♣ K♠ 6♥", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "J♥ J♣ 4♣ 4♠ 9♥", "Lose")>]
    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "4♥ 4♠ K♠ 10♦ 5♠", "Lose")>]

    // High card beats the right other high card hands:
    [<TestCase("A♥ J♥ 8♣ 7♦ 4♠", "K♥ J♠ 8♦ 7♠ 4♦", "Win")>] // Difference for highest card
    [<TestCase("A♥ J♥ 8♣ 7♦ 4♠", "A♣ 9♠ 8♦ 7♠ 4♦", "Win")>] // Difference on second card
    [<TestCase("A♥ J♥ 9♣ 7♦ 4♠", "A♣ J♠ 8♦ 7♠ 4♦", "Win")>] // Difference on third card
    [<TestCase("A♥ J♥ 9♣ 8♦ 4♠", "A♣ J♠ 9♦ 7♠ 4♦", "Win")>] // Difference on fourth card
    [<TestCase("A♥ J♥ 9♣ 8♦ 4♠", "A♣ J♠ 9♦ 8♠ 2♦", "Win")>] // Difference on fifth card

    // High card loses to the right other high card hands:
    [<TestCase("K♥ J♠ 8♦ 7♠ 4♦", "A♥ J♥ 8♣ 7♦ 4♠", "Lose")>] // Difference for highest card
    [<TestCase("A♣ 9♠ 8♦ 7♠ 4♦", "A♥ J♥ 8♣ 7♦ 4♠", "Lose")>] // Difference on second card
    [<TestCase("A♣ J♠ 8♦ 7♠ 4♦", "A♥ J♥ 9♣ 7♦ 4♠", "Lose")>] // Difference on third card
    [<TestCase("A♣ J♠ 9♦ 7♠ 4♦", "A♥ J♥ 9♣ 8♦ 4♠", "Lose")>] // Difference on fourth card
    [<TestCase("A♣ J♠ 9♦ 8♠ 2♦", "A♥ J♥ 9♣ 8♦ 4♠", "Lose")>] // Difference on fifth card

    // High card draws with the right other high card hands:
    [<TestCase("A♥ J♥ 9♣ 8♦ 4♠", "A♣ J♠ 9♦ 8♠ 4♦", "Draw")>] 

    member t.``the right outcome is correctly identified``(cs1 : string, cs2 : string, expected : string) =
        let hand1, hand2 = Hand.FromString(cs1), Hand.FromString(cs2)
        let actual = (CompareHands hand1 hand2).ToString()
        actual |> should equal expected

[<TestFixture>]
type ``Given various two-player rounds with an outright winner``() =

    [<TestCase("A♣ J♠ 9♦ 8♠ 4♦", "K♥ J♥ 9♣ 8♦ 4♠", 0)>] 
    [<TestCase("K♥ J♥ 9♣ 8♦ 4♠", "A♣ J♠ 9♦ 8♠ 4♦", 1)>] 

    [<TestCase("A♦ K♦ Q♦ J♦ 10♦", "K♥ J♥ 9♣ 8♦ 4♠", 0)>] 
    [<TestCase("K♥ J♥ 9♣ 8♦ 4♠", "A♦ K♦ Q♦ J♦ 10♦", 1)>] 
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, expectedIndex : int) =
        let hand1, hand2 = Hand.FromString(cs1), Hand.FromString(cs2)
        let actual = Showdown.FromHands [|hand1; hand2|]
        let expected = Winner expectedIndex
        actual |> should equal expected

[<TestFixture>]
type ``Given various three-player rounds with an outright winner``() =

    [<TestCase("A♣ J♠ 9♦ 8♠ 4♦", "K♥ J♥ 9♣ 8♦ 4♠", "J♦ 7♦ 5♥ 4♣ 3♥", 0)>] 
    [<TestCase("K♥ J♥ 9♣ 8♦ 4♠", "A♣ J♠ 9♦ 8♠ 4♦", "J♦ 7♦ 5♥ 4♣ 3♥", 1)>] 
    [<TestCase("K♥ J♥ 9♣ 8♦ 4♠", "J♦ 7♦ 5♥ 4♣ 3♥", "A♣ J♠ 9♦ 8♠ 4♦", 2)>] 
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, cs3 : string, expectedIndex : int) =
        let hand1, hand2, hand3 = Hand.FromString(cs1), Hand.FromString(cs2), Hand.FromString(cs3)
        let actual = Showdown.FromHands [|hand1; hand2; hand3|]
        let expected = Winner expectedIndex
        actual |> should equal expected

[<TestFixture>]
type ``Given various four-player rounds with an outright winner``() =

    [<TestCase("3♣ 3♠ 3♦ 6♣ 6♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Q♣ J♠ 10♠ 9♥ 8♥", "2♦ 2♠ 2♣ K♠ 6♥", 0)>] 
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "3♣ 3♠ 3♦ 6♣ 6♥", "Q♣ J♠ 10♠ 9♥ 8♥", "2♦ 2♠ 2♣ K♠ 6♥", 1)>] 
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♣ J♠ 10♠ 9♥ 8♥", "3♣ 3♠ 3♦ 6♣ 6♥", "2♦ 2♠ 2♣ K♠ 6♥", 2)>] 
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♣ J♠ 10♠ 9♥ 8♥", "2♦ 2♠ 2♣ K♠ 6♥", "3♣ 3♠ 3♦ 6♣ 6♥", 3)>] 
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, cs3 : string, cs4 : string, expectedIndex : int) =
        let hand1, hand2, hand3, hand4 = Hand.FromString(cs1), Hand.FromString(cs2), Hand.FromString(cs3), Hand.FromString(cs4)
        let actual = Showdown.FromHands [|hand1; hand2; hand3; hand4|]
        let expected = Winner expectedIndex
        actual |> should equal expected

[<TestFixture>]
type ``Given various five-player rounds with an outright winner``() =

    [<TestCase("9♣ 9♠ 9♦ 9♥ J♥", "J♥ J♣ 4♣ 4♠ 9♥", "4♥ 4♠ K♠ 10♦ 5♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "K♥ J♥ 8♣ 7♦ 4♠", 0)>] 
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "9♣ 9♠ 9♦ 9♥ J♥", "4♥ 4♠ K♠ 10♦ 5♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "K♥ J♥ 8♣ 7♦ 4♠", 1)>] 
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "4♥ 4♠ K♠ 10♦ 5♠", "9♣ 9♠ 9♦ 9♥ J♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "K♥ J♥ 8♣ 7♦ 4♠", 2)>] 
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "4♥ 4♠ K♠ 10♦ 5♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "9♣ 9♠ 9♦ 9♥ J♥", "K♥ J♥ 8♣ 7♦ 4♠", 3)>] 
    [<TestCase("J♥ J♣ 4♣ 4♠ 9♥", "4♥ 4♠ K♠ 10♦ 5♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "K♥ J♥ 8♣ 7♦ 4♠", "9♣ 9♠ 9♦ 9♥ J♥", 4)>] 
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, cs3 : string, cs4 : string, cs5 : string, expectedIndex : int) =
        let hand1, hand2, hand3, hand4, hand5 = Hand.FromString(cs1), Hand.FromString(cs2), Hand.FromString(cs3), Hand.FromString(cs4), Hand.FromString(cs5)
        let actual = Showdown.FromHands [|hand1; hand2; hand3; hand4; hand5|]
        let expected = Winner expectedIndex
        actual |> should equal expected


[<TestFixture>]
type ``Given various two-player rounds with no outright winner``() =

    [<TestCase("K♥ J♥ 8♣ 7♦ 4♠", "K♦ J♦ 8♦ 7♠ 4♥", "0,1")>]    
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 6♦ 4♦", "0,1")>]    
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, expectedIndexes : string) =
        let hand1, hand2 = Hand.FromString(cs1), Hand.FromString(cs2)
        let actual = Showdown.FromHands [|hand1; hand2|]
        let expected = Drawers (expectedIndexes.Split([|','|]) |> Array.map System.Int32.Parse)
        actual |> should equal expected

[<TestFixture>]
type ``Given various three-player rounds with no outright winner``() =

    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 6♦ 4♦", "K♠ J♠ 8♠ 7♥ 4♥", "0,1")>]    
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "K♠ J♠ 8♠ 7♥ 4♥", "Q♦ 10♦ 7♦ 6♦ 4♦", "0,2")>]    
    [<TestCase("K♠ J♠ 8♠ 7♥ 4♥", "Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 6♦ 4♦", "1,2")>]    
    [<TestCase("Q♦ 10♦ 7♦ 6♦ 4♦", "Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 6♦ 4♦", "0,1,2")>]    
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, cs3 : string, expectedIndexes : string) =
        let hand1, hand2, hand3 = Hand.FromString(cs1), Hand.FromString(cs2), Hand.FromString(cs3)
        let actual = Showdown.FromHands [|hand1; hand2; hand3|]
        let expected = Drawers (expectedIndexes.Split([|','|]) |> Array.map System.Int32.Parse)
        actual |> should equal expected

[<TestFixture>]
type ``Given various four-player rounds with no outright winner``() =

    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "Q♦ 10♦ 7♦ 6♦ 4♦", "K♠ J♠ 8♠ 7♥ 4♥", "6♥ 6♠ K♠ 10♥ 5♠", "0,1")>]    
    [<TestCase("Q♣ 10♣ 7♣ 6♣ 4♣", "K♠ J♠ 8♠ 7♥ 4♥", "6♥ 6♠ K♠ 10♥ 5♠", "Q♦ 10♦ 7♦ 6♦ 4♦", "0,3")>]    
    [<TestCase("K♠ 10♠ 5♠ 2♥ 2♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "Q♥ 10♥ 7♥ 6♥ 4♥", "Q♦ 10♦ 7♦ 6♦ 4♦", "1,2,3")>]    
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, cs3 : string, cs4 : string, expectedIndexes : string) =
        let hand1, hand2, hand3, hand4 = Hand.FromString(cs1), Hand.FromString(cs2), Hand.FromString(cs3), Hand.FromString(cs4)
        let actual = Showdown.FromHands [|hand1; hand2; hand3; hand4|]
        let expected = Drawers (expectedIndexes.Split([|','|]) |> Array.map System.Int32.Parse)
        actual |> should equal expected

[<TestFixture>]
type ``Given various five-player rounds with no outright winner``() =

    [<TestCase("K♠ 10♠ 5♠ 2♥ 2♠", "Q♣ 10♣ 7♣ 6♣ 4♣", "Q♥ 10♥ 7♥ 6♥ 4♥", "Q♦ 10♦ 7♦ 6♦ 4♦", "K♦ 9♦ 5♦ 2♣ 2♦", "1,2,3")>]    
    [<TestCase("K♠ 10♠ 5♠ 2♥ 2♠", "K♥ J♥ 8♣ 7♣ 4♠", "Q♥ 10♥ 7♥ 6♥ 4♥", "Q♦ 10♦ 7♦ 6♦ 4♦", "K♦ 9♦ 5♦ 2♣ 2♦", "2,3")>]    
    member t.``the right winner is correctly identified``(cs1 : string, cs2 : string, cs3 : string, cs4 : string, cs5 : string, expectedIndexes : string) =
        let hand1, hand2, hand3, hand4, hand5 = Hand.FromString(cs1), Hand.FromString(cs2), Hand.FromString(cs3), Hand.FromString(cs4), Hand.FromString(cs5)
        let actual = Showdown.FromHands [|hand1; hand2; hand3; hand4; hand5|]
        let expected = Drawers (expectedIndexes.Split([|','|]) |> Array.map System.Int32.Parse)
        actual |> should equal expected