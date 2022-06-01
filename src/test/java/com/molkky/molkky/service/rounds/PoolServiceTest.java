package com.molkky.molkky.service.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
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
 class PoolServiceTest {

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
    void testInsertTournamentWithOnePoolRound() {

        Tournament tournament = createTournament();

        tournament = createPool(tournament, 1, 2, 4 ,false, false);

        insertTeam(tournament, 8);


        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(2, tournament.getPhases().get(0).getRounds().size(),
                " there should be 2 rounds in the phase");
        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");
        Assertions.assertEquals(3, tournament.getTeams().get(0).getMatchs().size(),
                " There should be 3 matches per team ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getRounds().size(),
                " There should be 1 round per team ");
        Assertions.assertEquals(2, results.size(), " There should be 2 rounds of pool ");


        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.POOL, entry.getKey().getType(),
                    " The round should be of type pool ");
            Assertions.assertEquals(4, entry.getKey().getTeams().size(), " The  should be 4 teams");
            Assertions.assertEquals(6, entry.getValue().size(), " The  should be 6 matches");

        }

    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithOnePoolRound9Teams() {

        Tournament tournament = createTournament();

        tournament = createPool(tournament, 1, 2, 4, false, false);

        insertTeam(tournament, 9);


        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(2, tournament.getPhases().get(0).getRounds().size(),
                " there should be 2 rounds in the phase");
        Assertions.assertEquals(9, tournament.getTeams().size(), " There should be 9 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(4, tournament.getTeams().get(0).getMatchs().size(),
                " There should be 4 matches for  team 1");
        Assertions.assertEquals(4, tournament.getTeams().get(8).getMatchs().size(),
                " There should be 4 matches for  team 9 ");
        Assertions.assertEquals(3, tournament.getTeams().get(7).getMatchs().size(),
                " There should be 3 matches for  team 8 ");


        Assertions.assertEquals(1, tournament.getTeams().get(0).getRounds().size(),
                " There should be 1 round per team ");
        Assertions.assertEquals(2, results.size(), " There should be 2 rounds of pool ");


    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithTwoPools() {

        Tournament tournament = createTournament();

        tournament = createPool(tournament, 1, 2, 4, false, false);
        tournament = createPool(tournament, 2, 2, 4 ,false, false);

        insertTeam(tournament, 9);


        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());
        tournament.getTeams().get(0).setEliminated(true);
        Team t1 = teamRepository.save(tournament.getTeams().get(0));

        Map<Round, List<Match>> results2 =  phaseService.generate(tournament.getPhases().get(1).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Assertions.assertEquals(2, tournament.getPhases().size(), "Tournament should have 2 phases");

        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Pool,
                " It should be a instance of pool");
        Assertions.assertEquals(true, tournament.getPhases().get(1) instanceof Pool,
                " It should be a instance of pool");

        Assertions.assertEquals(2, tournament.getPhases().get(0).getRounds().size(),
                " there should be 2 rounds in the phase");

        Assertions.assertEquals(2, tournament.getPhases().get(1).getRounds().size(),
                " there should be 2 rounds in the phase");

        Assertions.assertEquals(9, tournament.getTeams().size(), " There should be 9 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");


        Assertions.assertEquals(1, tournament.getTeams().get(0).getRounds().size(),
                " There should be 1 round for team 1");
        Assertions.assertEquals(2, tournament.getTeams().get(1).getRounds().size(),
                " There should be 2 rounds per team 2");

        Assertions.assertEquals(5, tournament.getPhases().get(0).getRounds().get(0).getTeams().size()
                , " There should be 5 teams ");
        Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().get(1).getTeams().size()
                , " There should be 4 teams ");
        Assertions.assertEquals(4, tournament.getPhases().get(1).getRounds().get(0).getTeams().size()
                , " There should be 4 teams ");
        Assertions.assertEquals(4, tournament.getPhases().get(1).getRounds().get(1).getTeams().size()
                , " There should be 4 teams ");



        Assertions.assertEquals(2, results.size(), " There should be 2 rounds of pool 1 ");
        Assertions.assertEquals(2, results2.size(), " There should be 2 rounds of pool 2 ");


    }

    @Test
    @Rollback(false)
    @Transactional
    void validateMatchPool(){

        Tournament tournament = createTournament();

        tournament = createPool(tournament, 1, 2, 4, true, true);

        insertTeam(tournament, 6);

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
        List<Team> teams = teamRepository.findByTournamentAndEliminated(tournament,false);

        Assertions.assertEquals(6, tournament.getTeams().size(), "there should be 6 teams");

        Assertions.assertEquals(4, teams.size(), "there should be 4 teams remaining");

        Assertions.assertTrue( tournament.getTeams().get(4).isEliminated(), "TEAM 5 should be eliminated");
        Assertions.assertTrue( tournament.getTeams().get(5).isEliminated(), "TEAM 6 should be eliminated");

        Assertions.assertEquals(100, tournament.getTeams().get(0).getNbPoints(), "team 1 should have 100 points");
        Assertions.assertEquals(100, tournament.getTeams().get(1).getNbPoints(), "team 2 should have 100 points");

        Assertions.assertEquals(1, teams.get(0).getUserTournamentRoles().get(0).getNotifications().size(), "each player must have one notification");

    }



    Tournament createTournament(){
        Tournament tournament = new Tournament(
                "tournament test pool service",
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
    Tournament createPool(Tournament tournament, int nbPhase, int nbPool, int nbTeamsQualified , boolean ranking, boolean seedingSystem){
        Pool pool = new Pool();

        pool.setNbSets(1);
        pool.setVictoryValue(2);
        pool.setNbPhase(nbPhase);
        pool.setNbPools(nbPool);
        pool.setNbTeamsQualified(nbTeamsQualified);
        pool.setSeedingSystem(seedingSystem);
        pool.setRanking(ranking);

        pool.setTournament(tournament);
        pool =  phaseRepository.save(pool);

        tournament.getPhases().add(pool);
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
