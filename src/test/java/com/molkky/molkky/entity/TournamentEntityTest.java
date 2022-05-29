package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Finnish;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.UserRole;

import javax.transaction.Transactional;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class TournamentEntityTest {
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private SetRepository setRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private CourtRepository courtRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    @Test
    void testInsertTournament() {
        Tournament tournament = tournamentRepository.save(new Tournament(
                "tournament_name",
                "location",
                new Date(),
                new Date(),
                1,
                2,
                true,
                2,
                3,
                2
        ));

        Assertions.assertEquals("tournament_name", tournament.getName(), "Tournament name should be tournament_name");

        Tournament tournament2 = tournamentRepository.findById(tournament.getId());
        Assertions.assertEquals("tournament_name", tournament2.getName(), "Tournament name should be tournament_name");
    }

    @Test
    void testAmountOfPlayersPerTeam(){
        Tournament tournament = tournamentRepository.save(new Tournament(
                "tournament_name",
                "location",
                new Date(),
                new Date(),
                1,
                2,
                true,
                2,
                3,
                2
        ));
        tournament.setNbPlayersPerTeam(2);
        Assertions.assertEquals(2, tournament.getNbPlayersPerTeam(), "Amount of players per team should be 2");
    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithAdmins() {
        Tournament tournament = tournamentRepository.save(new Tournament(
                "tournament_name",
                "location",
                new Date(),
                new Date(),
                1,
                2,
                true,
                2,
                3,
                2
        ));
        User user = userRepository.save(new User("pseudoUser1", "surname1", "forename1", "club1", "email1"));
        List<UserTournamentRole> admins = new ArrayList<>();
        UserTournamentRole userTournamentRole = new UserTournamentRole();
        userTournamentRole.setUser(user);
        userTournamentRole.setRole(UserRole.ADM);
        admins.add(userTournamentRole);
        tournament.setUserTournamentRoles(admins);
        tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getUserTournamentRoles().size(), "Tournament should have 1 admin");
    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithRounds() {
        Tournament tournament = tournamentRepository.save(new Tournament(
                "tournament_name",
                "location",
                new Date(),
                new Date(),
                1,
                2,
                true,
                2,
                3,
                2
        ));

        Finnish finnish = new Finnish();
        finnish.setTournament(tournament);
        phaseRepository.save(finnish);
        List<Phase> phases = new ArrayList<>();
        phases.add(finnish);
        tournament.setPhases(phases);
        tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
    }

    @Test
    void testCreateTournamentWithModel() throws ParseException {
        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setName("tournament_name");
        tournamentModel.setLocation("location");
        tournamentModel.setDate("01-01-2020");
        tournamentModel.setCutOffDate("01-01-2020");
        tournamentModel.setMaxTeam(1);
        tournamentModel.setMinTeam(2);
        tournamentModel.setNbRounds(2);
        tournamentModel.setNbCourts(2);
        tournamentModel.setVisible(true);
        tournamentModel.setNbPlayersPerTeam(2);
        Tournament tournament = new Tournament(tournamentModel);
        Assertions.assertEquals("tournament_name", tournament.getName(), "Tournament name should be tournament_name");
        Assertions.assertEquals("location", tournament.getLocation(), "Tournament location should be location");
        Assertions.assertEquals(1, tournament.getMaxTeam(), "Tournament maxTeam should be 1");
        Assertions.assertEquals(2, tournament.getMinTeam(), "Tournament minTeam should be 2");
        Assertions.assertEquals(2, tournament.getNbRounds(), "Tournament nbRounds should be 2");
        Assertions.assertEquals(2, tournament.getNbCourts(), "Tournament nbCounts should be 2");
        Assertions.assertTrue(tournament.isVisible(), "Tournament visible should be true");
        Assertions.assertEquals(2, tournament.getNbPlayersPerTeam(), "Tournament nbPlayersPerTeam should be 2");

    }

    @Test
    void testCreateCompleteTournament(){
        Tournament tournament = new Tournament();
        tournament.setName("tournament_name_full");
        tournament.setLocation("location_full");
        tournament.setDate(new Date());
        tournament.setCutOffDate(new Date());
        tournament.setMaxTeam(2);
        tournament.setMinTeam(1);
        tournament.setNbRounds(1);
        tournament.setVisible(true);
        tournament.setNbPlayersPerTeam(2);
        tournament = tournamentRepository.save(tournament);

        Team team1 = new Team();
        team1.setName("team_name_1");
        team1.setTournament(tournament);
        teamRepository.save(team1);

        Team team2 = new Team();
        team2.setName("team_name_2");
        team2.setTournament(tournament);
        teamRepository.save(team2);

        List<Team> teams = new ArrayList<>();
        teams.add(team1);
        teams.add(team2);

        Match match = matchRepository.save(new Match());
        List<Set> sets = new ArrayList<>();
        Set set1 = new Set();
        set1.setTeams(teams);
        set1.setFinished(false);
        set1.setMatch(match);
        setRepository.save(set1);
        Set set2 = new Set();
        set2.setTeams(teams);
        set2.setFinished(false);
        set2.setMatch(match);
        setRepository.save(set2);
        Set set3 = new Set();
        set3.setTeams(teams);
        set3.setFinished(false);
        set3.setMatch(match);
        setRepository.save(set3);

        sets.add(set1);
        sets.add(set2);
        sets.add(set3);

        Round round = new Round();
        round.setTournament(tournament);
        //round.setPhase(new Phase());
        round = roundRepository.save(round);

        match.setTeams(teams);
        match.setFinished(false);
        match.setNbSets(3);
        match.setSets(sets);
        match.setCourt(courtRepository.save(new Court(true, "Bruh")));
        match.setRound(round);
        matchRepository.save(match);

        List<Match> matches = new ArrayList<>();
        matches.add(match);
        round.setMatches(matches);
        roundRepository.save(round);
        List<Round> rounds = new ArrayList<>();
        rounds.add(round);

        tournament.setRounds(rounds);
        tournamentRepository.save(tournament);

        Assertions.assertEquals("tournament_name_full", tournament.getName(), "Tournament name should be tournament_name_full");
    }
}
