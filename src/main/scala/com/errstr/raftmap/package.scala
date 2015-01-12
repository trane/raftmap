package com.errstr

package object raftmap {

  case class Term(value: Int) {
    assert(value >= 0)
    def +(inc: Int): Term =
      Term(value + inc)
  }

  object Term {
    val zero: Term = Term(0)
  }


  case class Index(value: Int) {
    assert(value >= 0)
  }

  type Logs[+A] = IndexedSeq[A]
  type Log = Logs[Term]
  type Servers = Set[Server]
  type Elections = List[Election]

  /**
   * History variables only used in the proof
   */
  trait HistoryVars {
    /**
     * Keeps track of successful elections, including the initial logs of the
     * leader and voters' logs. Set of functions containing various things about
     * successful elections (see BecomeLeader)
     */
    var elections = List[SuccessfulElection]
    /**
     * Keeps track of every log ever in the system (set of logs)
     */
    var allLogs = Set[Log]
    /**
     * History variable used in proof
     * Function from each server that voted for this candidate in its currentTerm
     * to that voter's log
     */
    def voterLog: Unit
  }

  // The set of server IDs
  // serverVars := delta[ <currentTerm, state, votedFor> ]
  trait ServerVars {
    // The server's term number
    val currentTerm: Term
    // The server's state (Follower, Candidate, or Leader)
    // probably don't need this (can make this server type be the state)
    val state: ServerState
    // The candidate server voted for in its current term, or None if it hasn't voted for any
    val votedFor: Option[Candidate]

  }

  // logVars := delta[ <log, commitIndex> ]
  trait LogVars {
    // The index into this seq is the index of the log entry
    // Unfortunately, the Sequence module defines Head(s) as the entry
    // with index 1, so be careful not to use that
    val log: Log = IndexedSeq(Term.zero)

    // The index of the latest entry in the log the state machine may apply
    val commitIndex: Index = Index(0)
  }

  trait CandidateVars {
    /**
     * The set of servers from which the candidate has received a RequestVote
     * response in its "currentTerm"
     */
    val votesResponded: Set[Server]

    /**
     * The set of servers from which the candidate has received a vote in its
     * currentTerm
     */
    val votesGranted: Set[Server]
  }

  trait LeaderVars {
    // The next entry to send to each follower
    val nextIndex: Index
    /**
     * The latest entry that each follower has acknowledged is the same as the
     * leader's. This is used to calculate commitIndex on the leader
     */
    val matchIndex: Index

    val elections: Elections
  }


  // The set of requests that can go into the log
  trait Value

  /*
  trait Server extends ServerVars with LogVars {
    val currentTerm = Term(1)
    val state = Follower
    val votedFor = None
  }
  */

  case class Server(currentTerm: Term = Term(1),
                    state: ServerState = Follower,
                    votedFor: Option[Candidate] = None) extends ServerVars with LogVars

  trait ServerState
  case class Follower() extends ServerState
  case class Candidate(votesResponded: Set[Server] = {},
                       votesGranted: Set[Server] = {}) extends ServerState with CandidateVars
  case class Leader(nextIndex: Index = Index(1),
                    matchIndex: Index = Index(0),
                    elections: Elections) extends ServerState with LeaderVars

  case object Nil

  sealed trait MessageType
  case object RequestVote extends MessageType
  case object AppendEntries extends MessageType

  sealed trait Message
  class Request[MessageType] extends Message
  class Response[MessageType] extends Message

  /*
  A bag of records representing requests and responses sent from one server
  to another.
   */
  type Bag[+A] = IndexedSeq[A]
  type Messages = Bag[Message]
  implicit class MessageHelper(val messages: Messages) extends AnyVal {
    def send(m: Message): Messages =
      WithMessage(m, messages)
    def discard(m: Message): Messages =
      WithoutMessage(m, messages)
    def reply(response: Response, request: Request) =
      WithoutMessage(request, WithMessage(response, messages))
  }

  trait Election {
    val leaderLogs = List[Log]
    val votersLogs = List[Log]
  }
  case class SuccessfulElection() extends Election
  case class FailedElection() extends Election

  /**
   * All variables; used for stuttering (asserting state hasn't changed)
   */
  trait Vars {
    val messages: Messages
    val allLogs: List[Log]
    val serverVars: ServerVars
    val candidateVars: CandidateVars
    val leaderVars: LeaderVars
    val logVars: LogVars
  }


  /**
   * The set of all quorums. This just calculates simple majorities, but the only
   * important property is that every quorum overlaps with every other
   */
  object Quorum {
    // {i ∈ SUBSET (Server) : Cardinality(i) * 2 > Cardinality (Server)}
    def apply(servers: Servers) = for {
      i <- servers.subsets
      if (Cardinality(i) * 2 > Cardinality(servers))
    } yield i
  }

  object Cardinality {
    def apply(servers: Servers): Int =
      servers.size
  }

  object LastTerm {
    def apply(xlog: Log): Term =
      if (xlog.isEmpty) Term.zero
      else xlog.last

  }

  object WithMessage {
    def apply(m: Message, msgs: Messages): Messages =
      msgs.seq :+ m
  }

  object WithoutMessage {
    def apply(m: Message, msgs: Messages): Messages =
      msgs.seq filter (_ == m)
  }

  object Restart {
    def apply(i: Server): Server =
      new Server(currentTerm = i.currentTerm, votedFor = i.votedFor) {
        override val log = i.log
      }
  }

  object Timeout {
    def apply(i: Server): Server =
      new Server(state = Candidate(), currentTerm = i.currentTerm + 1, votedFor = None)
  }

  object RequestVote {
    def apply(i: Server, j: Server): Response[RequestVote] =
      ???
  }
}
