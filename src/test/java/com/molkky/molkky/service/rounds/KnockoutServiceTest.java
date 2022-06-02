package com.molkky.molkky.service.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.PhaseService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.PhaseType;
import type.TournamentStatus;
import type.UserRole;

import javax.transaction.Transactional;
import java.util.*;

@SpringBootTest(classes = MolkkyApplication.class)
 class KnockoutServiceTest {

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    @Autowired
    private TeamRepository teamRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private PhaseService phaseService;

    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private MatchService matchService;


    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithKnockoutRound() {

        Tournament tournament = createTournament();

        tournament = createKnockout(tournament, 1, 1);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Knockout,
                " It should be a instance of a simple game");

        Assertions.assertEquals(1, tournament.getPhases().get(0).getRounds().size(),
                " there should be 1 round in the phase");

        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(1, results.size(), " There should be 4 rounds of simple game ");

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.KNOCKOUT, entry.getKey().getType(),
                    " The round should be of type knockout ");
            Assertions.assertEquals(8, entry.getKey().getTeams().size(), " The  should be 2 teams");
            Assertions.assertEquals(4, entry.getValue().size(), " The  should be one match");

        }

    }



    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithKnockout2Rounds() {

        Tournament tournament = createTournament();

        tournament = createKnockout(tournament, 1, 1);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());


        List<Round> rounds = new ArrayList<>(tournament.getPhases().get(0).getRounds());
        for (Round r: rounds){
            for(Match m : r.getMatches()){
                Random rand = new Random();
                m.setFinished(true);
                m.setWinner(m.getTeams().get(0));
                m.setScoreTeam1(50);
                m.setScoreTeam2(rand.nextInt(49));
                matchRepository.save(m);
                matchService.validateMatch(m);
            }
        }

        Map<Round, List<Match>> results2 =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Round r = tournament.getPhases().get(0).getRounds().get(1);

        List<Match> matches = results2.get(r);

        Assertions.assertEquals(PhaseType.KNOCKOUT, r.getType(),
                " The round should be of type knockout ");
        Assertions.assertEquals(4, r.getTeams().size(), " The  should be 4 teams");
        Assertions.assertEquals(2, matches.size(), " The  should be 2 matches");
        Assertions.assertEquals(matches.get(0).getTeams().get(0), matches.get(0).getWinner(), " first team is the winner");
        Assertions.assertEquals(50, tournament.getTeams().get(0).getNbPoints() ," Team 1 should have 50 points");
        Assertions.assertEquals(50, tournament.getTeams().get(2).getNbPoints() ," Team 3 should have 50 points");



    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithKnockoutEnd() {

        Tournament tournament = createTournament();

        tournament = createKnockout(tournament, 1, 1);

        insertTeam(tournament, 4);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Round round= tournament.getPhases().get(0).getRounds().get(0);

            for(Match m : round.getMatches()){
                m.setFinished(true);
                m.setWinner(m.getTeams().get(0));
                m.setScoreTeam1(50);
                m.setScoreTeam2(0);
                matchRepository.save(m);
                matchService.validateMatch(m);
            }

        tournament = tournamentRepository.findById(tournament.getId());

        Map<Round, List<Match>> results2 =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        Round round2= tournament.getPhases().get(0).getRounds().get(1);

        for(Match m : round2.getMatches()){
            m.setFinished(true);
            m.setWinner(m.getTeams().get(0));
            m.setScoreTeam1(50);
            m.setScoreTeam2(0);
            matchRepository.save(m);
            matchService.validateMatch(m);
        }

        tournament = tournamentRepository.findById(tournament.getId());

        Assertions.assertTrue(tournament.getPhases().get(0).getFinished(), "Phase should be finished");
        Assertions.assertEquals(100, tournament.getTeams().get(0).getNbPoints() ," Team 1 should have 100 points");

        List<Team> teams = teamRepository.findByTournamentAndEliminated(tournament,false);

        Assertions.assertEquals(1, teams.size()," There should be one team remaining");

        tournamentRepository.delete(tournament);

    }


    Tournament createTournament(){
        Tournament tournament = new Tournament(
                "tournament test",
                "location",
                new Date(),
                new Date(),
                1,
                8,
                true,
                2,
                3,
                2
        );
        tournament.setNbPlayersPerTeam(1);
        tournament.setVisible(true);
        tournament.setStatus(TournamentStatus.AVAILABLE);
        return tournamentRepository.save(tournament);

    }
    Tournament createKnockout(Tournament tournament, int nbPhase, int nbMatch){

        Knockout knockout = new Knockout();
        knockout.setNbPhase(nbPhase);
        knockout.setNbSets(3);
        knockout.setNbMatch(nbMatch);
        knockout.setTournament(tournament);
        knockout.setRanking(true);
        knockout.setSeedingSystem(true);


        knockout =  phaseRepository.save(knockout);

        tournament.getPhases().add(knockout);

        return tournamentRepository.save(tournament);
    }

    void insertTeam(Tournament tournament, int qtd) {
        for (int i = 1; i <= qtd; i++) {
            Team team = new Team();

            team.setCode("12345");

            team.setName("Team" + i);
            team.setTournament(tournament);


            tournament.getTeams().add(team);

            User player = new User();

            player.setForename("User" + i);

            player = userRepository.save(player);

            UserTournamentRole userTournamentRole = new UserTournamentRole();

            userTournamentRole.setRole(UserRole.PLAYER);
            userTournamentRole.setUser(player);
            userTournamentRole.setTournament(tournament);
            userTournamentRole.setTeam(team);

            player.getUserTournamentRoles().add(userTournamentRole);
            tournament.getUserTournamentRoles().add(userTournamentRole);
            team.getUserTournamentRoles().add(userTournamentRole);

            team = teamRepository.save(team);
            userTournamentRoleRepository.save(userTournamentRole);
            tournamentRepository.save(tournament);
            userRepository.save(player);
        }
    }

}
