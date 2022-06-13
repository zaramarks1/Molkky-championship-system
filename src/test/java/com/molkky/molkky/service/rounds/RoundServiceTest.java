package com.molkky.molkky.service.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.PhaseService;
import com.molkky.molkky.service.RoundService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import type.PhaseType;
import type.TournamentStatus;
import type.UserRole;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

@SpringBootTest(classes = MolkkyApplication.class)
 class RoundServiceTest {

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

    @Autowired
    private RoundService roundService;


    @Test
    void assignRandomStaffToMatchSimple() {

        Tournament tournament = createTournament();

        tournament = createSimpleGame(tournament, 1, false, false, 1);

        insertTeam(tournament, 8);

        insertStaff(tournament, 2);

        Map<Round, List<Match>> results = phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());
        for (Round r : tournament.getPhases().get(0).getRounds()) {
            for (Match m : r.getMatches()) {
                Assertions.assertNotNull(m.getUser(), "there should be a staff assign");
            }
        }
    }

        @Test
        @Rollback(false)
        @Transactional
        void assignRandomStaffToMatchPool(){

            Tournament tournament = createTournament();

            tournament = createPool(tournament, 1, 2, 4 ,false, false);

            insertTeam(tournament, 8);

            insertStaff(tournament, 2);

            Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

            tournament = tournamentRepository.findById(tournament.getId());
            for(Round r : tournament.getPhases().get(0).getRounds()){
                for(Match m : r.getMatches()){
                    Assertions.assertNotNull(m.getUser(), "there should be a staff assign");
                }
            }

        }

    @Test
    @Rollback(false)
    @Transactional
    void assignRandomStaffToMatchKnockout(){

        Tournament tournament = createTournament();

        tournament = createKnockout(tournament, 1);

        insertTeam(tournament, 8);

        insertStaff(tournament, 2);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());
        for(Round r : tournament.getPhases().get(0).getRounds()){
            for(Match m : r.getMatches()){
                Assertions.assertNotNull(m.getUser(), "there should be a staff assign");
            }
        }

    }


    @Test
    @Rollback(false)
    @Transactional
    void assignRandomStaffToMatchSwiss(){

        Tournament tournament = createTournament();

        tournament = createSwiss(tournament, 1, 3, 2);

        insertTeam(tournament, 8);

        insertStaff(tournament, 2);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());
        for(Round r : tournament.getPhases().get(0).getRounds()){
            for(Match m : r.getMatches()){
                Assertions.assertNotNull(m.getUser(), "there should be a staff assign");
            }
        }

    }



    Tournament createTournament(){
        Tournament tournament = new Tournament(
                "tournament test simple game service",
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
    Tournament createSimpleGame(Tournament tournament, int nbPhase, boolean ranking, boolean seedingSystem, int nbQualifies){
        SimpleGame simpleGame = new SimpleGame();
        simpleGame.setNbSets(3);
        simpleGame.setTournament(tournament);
        simpleGame.setNbPhase(nbPhase);
        simpleGame.setRanking(ranking);
        simpleGame.setSeedingSystem(seedingSystem);
        simpleGame.setNbTeamsQualified(nbQualifies);
        simpleGame.setRandomStaff(true);

        simpleGame =  phaseRepository.save(simpleGame);

        tournament.getPhases().add(simpleGame);

        return tournamentRepository.save(tournament);
    }

    Tournament createSwiss(Tournament tournament, int nbPhase, int nbMatch, int nbQualified){

        SwissPool swissPool = new SwissPool();
        swissPool.setNbPhase(nbPhase);
        swissPool.setNbSets(3);
        swissPool.setNbSubRounds(nbMatch);
        swissPool.setTournament(tournament);
        swissPool.setNbTeamsQualified(nbQualified);
        swissPool.setRanking(true);
        swissPool.setSeedingSystem(true);
        swissPool.setRandomStaff(true);

        swissPool =  phaseRepository.save(swissPool);

        tournament.getPhases().add(swissPool);

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
        pool.setRandomStaff(true);

        pool.setTournament(tournament);
        pool =  phaseRepository.save(pool);

        tournament.getPhases().add(pool);
        return tournamentRepository.save(tournament);
    }
    Tournament createKnockout(Tournament tournament, int nbPhase){

        Knockout knockout = new Knockout();
        knockout.setNbPhase(nbPhase);
        knockout.setNbSets(3);
        knockout.setTournament(tournament);
        knockout.setRanking(true);
        knockout.setSeedingSystem(true);
        knockout.setRandomStaff(true);


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

    void insertStaff(Tournament tournament, int qtd){
        for (int i = 1; i <= qtd; i++) {
            User staff = new User();

            staff.setForename("Staff" + i);

            staff = userRepository.save(staff);

            UserTournamentRole userTournamentRole = new UserTournamentRole();

            userTournamentRole.setRole(UserRole.STAFF);
            userTournamentRole.setUser(staff);
            userTournamentRole.setTournament(tournament);

            staff.getUserTournamentRoles().add(userTournamentRole);
            tournament.getUserTournamentRoles().add(userTournamentRole);

            userTournamentRoleRepository.save(userTournamentRole);
            tournamentRepository.save(tournament);
            userRepository.save(staff);
        }
    }
}
