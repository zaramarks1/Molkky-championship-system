package com.molkky.molkky.service.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.PhaseService;
import com.molkky.molkky.service.SwissService;
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

 class SwissServiceTest {

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
    void testInsertTournamentWithSwissRound() {

        Tournament tournament = createTournament();

        tournament = createSwiss(tournament, 1, 1);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof SwissPool,
                " It should be a instance of a swiss");

        Assertions.assertEquals(1, tournament.getPhases().get(0).getRounds().size(),
                " there should be 1 round in the phase");

        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(1, results.size(), " There should be 4 rounds of swiss ");

        for(Match m : tournament.getPhases().get(0).getRounds().get(0).getMatches()){
            Random rand = new Random();
            m.setFinished(true);
            m.setWinner(m.getTeams().get(0));
            m.setScoreTeam1(50);
            m.setScoreTeam2(rand.nextInt(49));
            matchRepository.save(m);
            matchService.validateMatch(m);
        }
        tournament = tournamentRepository.findById(tournament.getId());

        SwissPool swissPool = (SwissPool) tournament.getPhases().get(0);
        Assertions.assertEquals(1, swissPool.getIndexSubRound(), " Index 1");
        Assertions.assertEquals(1, swissPool.getNbSubRounds(), " Quantite de sub rounds :  1");

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.SWISSPOOL, entry.getKey().getType(),
                    " The round should be of type swiss ");
            Assertions.assertEquals(8, entry.getKey().getTeams().size(), " The  should be 8 teams");
            Assertions.assertEquals(4, entry.getValue().size(), " The  should be 4 match");

        }



        Map<Round, List<Match>> results2 =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        Assertions.assertNull(results2, "results 2 ne doit pas exister");
    }



    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithSwiss2Rounds() {

        Tournament tournament = createTournament();

        tournament = createSwiss(tournament, 1, 3);

        insertTeam(tournament, 4);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());


        for(Match m : tournament.getPhases().get(0).getRounds().get(0).getMatches()){
            Random rand = new Random();
            m.setFinished(true);
            m.setWinner(m.getTeams().get(0));
            m.setScoreTeam1(50);
            m.setScoreTeam2(rand.nextInt(49));
            matchRepository.save(m);
            matchService.validateMatch(m);
        }
        Map<Round, List<Match>> results2 =  phaseService.generate(tournament.getPhases().get(0).getId().toString());


        tournament = tournamentRepository.findById(tournament.getId());

        Round r = tournament.getPhases().get(0).getRounds().get(1);

        List<Match> matches = results2.get(r);

        Assertions.assertEquals(PhaseType.SWISSPOOL, r.getType(),
                " The round should be of type swiss ");
        Assertions.assertEquals(4, r.getTeams().size(), " The  should be 4 teams");
        Assertions.assertEquals(2, matches.size(), " The  should be 2 matches");
        Assertions.assertEquals(50, tournament.getTeams().get(0).getNbPoints() ," Team 1 should have 50 points");
        Assertions.assertEquals(50, tournament.getTeams().get(2).getNbPoints() ," Team 3 should have 50 points");

        SwissPool swissPool = (SwissPool) tournament.getPhases().get(0);
        Assertions.assertEquals(2, swissPool.getIndexSubRound(), " Index 2");
        Assertions.assertEquals(3, swissPool.getNbSubRounds(), " Quantite de sub rounds :  3");

        for(Match m : tournament.getPhases().get(0).getRounds().get(1).getMatches()){
            Random rand = new Random();
            m.setFinished(true);
            m.setWinner(m.getTeams().get(0));
            m.setScoreTeam1(50);
            m.setScoreTeam2(rand.nextInt(49));
            matchRepository.save(m);
            matchService.validateMatch(m);
        }

        // Assertions.assertEquals(List.of(tournament.getTeams().get(0), tournament.getTeams().get(1)), matches.get(0).getTeams(), " TEAM 1 AND TEAM 3 should be playing");





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
                true,
                2,
                3,
                2
        );
        tournament.setNbPlayersPerTeam(1);
        tournament.setVisible(true);
        tournament.setStatus(TournamentStatus.INPROGRESS);
        tournament.setIndexPhase(1);
        return tournamentRepository.save(tournament);

    }
    Tournament createSwiss(Tournament tournament, int nbPhase, int nbMatch){

        SwissPool swissPool = new SwissPool();
        swissPool.setNbPhase(nbPhase);
        swissPool.setNbSets(3);
        swissPool.setNbSubRounds(nbMatch);
        swissPool.setTournament(tournament);
        swissPool.setRanking(true);
        swissPool.setSeedingSystem(true);

        swissPool =  phaseRepository.save(swissPool);

        tournament.getPhases().add(swissPool);

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
