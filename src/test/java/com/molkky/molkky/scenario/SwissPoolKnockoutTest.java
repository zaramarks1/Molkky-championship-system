package com.molkky.molkky.scenario;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.service.pool.SwissPoolService;
import com.molkky.molkky.service.scenario.PoolKnockoutScenario;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class SwissPoolKnockoutTest {
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private SwissPoolService swissPoolService;
    @Autowired
    private PoolKnockoutScenario scenario;

    @Test
    void testScenario(){
//        creation tournoi et ajout des equipes
        Tournament scenarioTournament = new Tournament();
        List<Team> teams = generateRandomTeams(4);
        scenarioTournament.setTeams(teams);
        scenarioTournament.setName("Tournoi de test knock out avec swissPool");
        scenario.create(scenarioTournament);
//        test nom
        Assertions.assertEquals("Tournoi de test knock out avec swissPool", scenarioTournament.getName());
//        test nombre équipe
        Assertions.assertEquals(4, scenarioTournament.getTeams().size());
//        test nombre rounds
        Assertions.assertEquals(2, scenarioTournament.getRounds().size());

//        swissPool puis kncokout
        Assertions.assertEquals("swissPool", scenarioTournament.getRounds().get(0).getType());
        Assertions.assertEquals("knockOut", scenarioTournament.getRounds().get(1).getType());
//        nombre de rounds
        Assertions.assertEquals(2, scenarioTournament.getRounds().size());
        scenario.start(scenarioTournament);
//        nombre de matchs
//        12 matchs dans la pool à 4 équipes
        Assertions.assertEquals(12, scenario.getCurrentPhaseMatches(scenarioTournament).size());

//        faire gagner les 2 premieres equipes
        for (Match match: scenarioTournament.getRounds().get(0).getSwissPool().getMatches()) {
            scenario.setMatchScore(match, 50, 0, scenarioTournament);
        }

//        la pool est finie
        Assertions.assertTrue(scenarioTournament.getRounds().get(0).getSwissPool().getFinished());
        Assertions.assertEquals(1 ,scenarioTournament.getIndexPhase());
//        vérifier que la nouvelle pool (knockOut) contient deux matchs car on a prit que les deux premières equipes
        Assertions.assertEquals(2, scenarioTournament.getRounds().get(1).getKnockout().getTeamsRemaining());
        Assertions.assertEquals(1, scenario.getCurrentPhaseMatches(scenarioTournament).size());
    }

    @Test
    void createTournamentWithNTeams(){
        // given
        Tournament tournament = new Tournament();
        List<Team> teams = generateRandomTeams(8);
        tournament.setTeams(teams);
        tournament.setName("Tournoi de test knock out avec swissPool 8 teams");

        // when
        scenario.create(tournament);
        scenario.start(tournament);

        // then
        Assertions.assertNotNull(tournament.getId());
        Assertions.assertEquals(8, tournament.getTeams().size());
        Assertions.assertEquals(3, tournament.getRounds().size());
        Assertions.assertEquals("swissPool", tournament.getRounds().get(0).getType());
        Assertions.assertEquals("swissPool", tournament.getRounds().get(1).getType());
        Assertions.assertEquals("knockOut", tournament.getRounds().get(2).getType());
        Assertions.assertEquals(2, scenario.getSwissPools(tournament).size());
        Assertions.assertEquals(12, scenario.getSwissPools(tournament).get(0).getMatches().size());
        Assertions.assertEquals(12, scenario.getSwissPools(tournament).get(1).getMatches().size());
        Assertions.assertEquals(24, scenario.getCurrentPhaseMatches(tournament).size());

        //        faire gagner les 2 premieres equipes
        for (Match match: scenario.getSwissPools(tournament).get(0).getMatches()) {
            scenario.setMatchScore(match, 50, 0, tournament);
            Assertions.assertTrue(match.getFinished());
        }
        for (Match match: scenario.getSwissPools(tournament).get(1).getMatches()) {
            scenario.setMatchScore(match, 0, 50, tournament);
            Assertions.assertTrue(match.getFinished());
        }
        Assertions.assertTrue(swissPoolService.areAllMatchesFinished(scenario.getSwissPools(tournament).get(0)));
        Assertions.assertTrue(swissPoolService.areAllMatchesFinished(scenario.getSwissPools(tournament).get(1)));
        Assertions.assertTrue(scenario.areAllSwissPoolsFinished(tournament));
//        la pool est finie
        Assertions.assertTrue(scenario.getSwissPools(tournament).get(0).getFinished());
        Assertions.assertTrue(scenario.getSwissPools(tournament).get(1).getFinished());
        Assertions.assertEquals(1 ,tournament.getIndexPhase());
//        vérifier que la nouvelle pool (knockOut) contient deux matchs car on a prit que les deux premières equipes
        Assertions.assertEquals(4, scenario.getKnockout(tournament).getTeamsRemaining());
        Assertions.assertEquals(2, scenario.getCurrentPhaseMatches(tournament).size());

//        play knockout matches
        for (Match match: scenario.getKnockout(tournament).getMatches()) {
            scenario.setMatchScore(match, 50, 0, tournament);
        }
        Assertions.assertEquals(2, scenario.getKnockout(tournament).getTeamsRemaining());
        Assertions.assertEquals(3, scenario.getCurrentPhaseMatches(tournament).size());
        Assertions.assertEquals(false, scenario.getKnockout(tournament).getFinished());

//        play matches that aren't finished
        for (Match match: scenario.getCurrentPhaseMatches(tournament)) {
            if(!match.getFinished()) {
                scenario.setMatchScore(match, 50, 0, tournament);
            }
        }

//        check additionnal matches aren't generated bc 1 team is remaining
        Assertions.assertEquals(3, scenario.getCurrentPhaseMatches(tournament).size());
        Assertions.assertEquals(1, scenario.getKnockout(tournament).getTeamsRemaining());
        Assertions.assertTrue(scenario.getKnockout(tournament).getFinished());
        Assertions.assertTrue(tournament.isFinished());
    }

    @Test
    void createTest(){
        // given
        Tournament tournament = new Tournament();
        List<Team> teams = generateRandomTeams(4);
        tournament.setTeams(teams);
        tournament.setName("Tournoi de test knock out avec swissPool");

        // when
        Tournament tournamentResult = scenario.create(tournament);

        // then
        Assertions.assertNotNull(tournament.getId());

    }

    @Test
    void goToNextPhaseTest(){
        Tournament tournament = new Tournament();
        List<Team> teams = generateRandomTeams(4);
        tournament.setTeams(teams);
        tournament.setName("Tournoi de test knock out avec swissPool");
        scenario.create(tournament);
        Assertions.assertEquals(0, tournament.getIndexPhase());
        scenario.goToNextPhase(tournament);
        Assertions.assertEquals(1, tournament.getIndexPhase());
        scenario.goToNextPhase(tournament);
        Assertions.assertTrue(tournament.isFinished());
    }

    List<Team> generateRandomTeams(int nbTeams){
        List<Team> teams = new ArrayList<>();
        for (int i = 0; i < nbTeams; i++) {
            Team team = new Team();
            teams.add(teamRepository.save(team));
        }
        return teams;
    }
}
