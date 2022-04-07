package com.molkky.molkky.scenario;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TeamRepository;
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
    private PoolKnockoutScenario scenario;


    @Test
    void testScenario(){
//        creation tournoi et ajout des equipes
        Tournament scenarioTournament = new Tournament();
        List<Team> teams = generateRandomTeams(4);
        scenarioTournament.setTeams(teams);
        scenarioTournament.setName("Tournoi de test knock out avec swissPool");
        scenario.init(scenarioTournament);
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
//        vérifier que la nouvelle pool (knockOut) contient deux matchs car on a prit que les deux premières equipes
        Assertions.assertEquals(2, scenarioTournament.getRounds().get(1).getKnockout().getMatches().size());
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
