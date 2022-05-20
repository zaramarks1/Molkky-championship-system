package com.molkky.molkky.service.rounds;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.repository.*;
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
import java.util.Date;
import java.util.List;
import java.util.Map;

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

        Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().size(),
                " there should be 4 rounds in the phase");

        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(4, results.size(), " There should be 4 rounds of simple game ");

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.KNOCKOUT, entry.getKey().getType(),
                    " The round should be of type knockout ");
            Assertions.assertEquals(2, entry.getKey().getTeams().size(), " The  should be 2 teams");
            Assertions.assertEquals(1, entry.getValue().size(), " The  should be one match");

        }

    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWithKnockoutRoundWith2matches() {

        Tournament tournament = createTournament();

        tournament = createKnockout(tournament, 1, 2);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament = tournamentRepository.findById(tournament.getId());

        Assertions.assertEquals(1, tournament.getPhases().size(), "Tournament should have 1 phase");
        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Knockout,
                " It should be a instance of a knockoput");

        Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().size(),
                " there should be 4 rounds in the phase");

        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 8 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");

        Assertions.assertEquals(4, results.size(), " There should be 4 rounds of simple game ");

        Assertions.assertEquals(2, tournament.getPhases().get(0).getRounds().get(0).getMatches().size(),
                " there should be 2 matches in the first round");
        Assertions.assertEquals(2, tournament.getPhases().get(0).getRounds().get(3).getMatches().size(),
                " there should be 2 matches in the last");

        Assertions.assertEquals(2, tournament.getTeams().get(5).getMatchs().size(), " There should be 2 matches for team 6 ");
        Assertions.assertEquals(2, tournament.getTeams().get(7).getMatchs().size(), " There should be 2 matches for team 8 ");
        Assertions.assertEquals(2, tournament.getTeams().get(6).getMatchs().size(), " There should be 2 matches for team 7 ");

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){

            Assertions.assertEquals(PhaseType.KNOCKOUT, entry.getKey().getType(),
                    " The round should be of type knockout ");
            Assertions.assertEquals(2, entry.getKey().getTeams().size(), " The  should be 2 teams");
            Assertions.assertEquals(2, entry.getValue().size(), " The  should be two matches");

        }

    }

    @Test
    @Rollback(false)
    @Transactional
    void testInsertTournamentWit2hKnockoutRound() {

        Tournament tournament = createTournament();

        tournament = createKnockout(tournament, 1, 1);
        tournament = createKnockout(tournament, 2, 1);

        insertTeam(tournament, 8);

        Map<Round, List<Match>> results =  phaseService.generate(tournament.getPhases().get(0).getId().toString());

        tournament.getTeams().get(0).setEliminated(true);
        tournament.getTeams().get(1).setEliminated(true);
        tournament.getTeams().get(2).setEliminated(true);
        tournament.getTeams().get(3).setEliminated(true);

        tournament = tournamentRepository.save(tournament);

        Map<Round, List<Match>> results2 =  phaseService.generate(tournament.getPhases().get(1).getId().toString());

        Assertions.assertEquals(2, tournament.getPhases().size(), "Tournament should have 2 phases");

        Assertions.assertEquals(true, tournament.getPhases().get(0) instanceof Knockout,
                " It should be a instance of knockout");
        Assertions.assertEquals(true, tournament.getPhases().get(1) instanceof Knockout,
                " It should be a instance of knockout");

        Assertions.assertEquals(4, tournament.getPhases().get(0).getRounds().size(),
                " there should be 4 rounds in the phase 1");

        Assertions.assertEquals(2, tournament.getPhases().get(1).getRounds().size(),
                " there should be 2 rounds in the phase 2");

        Assertions.assertEquals(8, tournament.getTeams().size(), " There should be 9 teams ");
        Assertions.assertEquals(1, tournament.getTeams().get(0).getUserTournamentRoles().size(),
                " There should be 1 player per team ");


        Assertions.assertEquals(1, tournament.getTeams().get(0).getRounds().size(),
                " There should be 1 round for team 1");
        Assertions.assertEquals(2, tournament.getTeams().get(4).getRounds().size(),
                " There should be 2 rounds per team 4");

        Assertions.assertEquals(4, results.size(), "there should be 4 rounds for phase 1");
        Assertions.assertEquals(2, results2.size(), "there should be two rounds for phase 2");
        for(Map.Entry<Round, List<Match>> entry : results2.entrySet()){

            Assertions.assertEquals(PhaseType.KNOCKOUT, entry.getKey().getType(),
                    " The round should be of type knockout ");
            Assertions.assertEquals(2, entry.getKey().getTeams().size(), " The  should be 2 teams");
            Assertions.assertEquals(1, entry.getValue().size(), " The  should be one match");

        }


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
