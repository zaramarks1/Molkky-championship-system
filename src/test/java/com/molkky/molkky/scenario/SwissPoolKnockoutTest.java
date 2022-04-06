package com.molkky.molkky.scenario;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.service.PoolKnockoutScenarioService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.HashSet;
import java.util.Set;

@SpringBootTest(classes = MolkkyApplication.class)
class SwissPoolKnockoutTest {
    @Autowired
    private TeamRepository teamRepository;

    @Test
    void testScenario(){
//        creation tournoi et ajout des equipes
        PoolKnockoutScenarioService scenario = new PoolKnockoutScenarioService();
        Tournament scenarioTournament = scenario.getTournament();
        scenarioTournament.setTeams(generateRandomTeams(4));
        scenarioTournament.setName("Tournoi de test knock out avec swissPool");
        scenario.init();
//        test nom
        Assertions.assertEquals("Tournoi de test knock out avec swissPool", scenario.getTournament().getName());
//        test nombre équipe
        Assertions.assertEquals(4, scenario.getTournament().getTeams().size());
//        test nombre rounds
        Assertions.assertEquals(2, scenario.getTournament().getRounds().size());
        Assertions.assertEquals("swissPool", scenario.getTournament().getRounds().iterator().next().getType());
        Assertions.assertEquals("knockOut", scenario.getTournament().getRounds().iterator().next().getType());
//        scenario.start();

    }

    Set<Team> generateRandomTeams(int nbTeams){
        Set<Team> teams = new HashSet<>();
        for (int i = 0; i < nbTeams; i++) {
            Team team = new Team();
            teams.add(teamRepository.save(team));
        }
        return teams;
    }
}
