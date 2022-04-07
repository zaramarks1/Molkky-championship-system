package com.molkky.molkky.scenario;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TeamRepository;
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
    @Autowired
    private PoolKnockoutScenario scenario;


    @Test
    void testScenario(){
//        creation tournoi et ajout des equipes
        Tournament scenarioTournament = new Tournament();
        scenarioTournament.setTeams(generateRandomTeams(4));
        scenarioTournament.setName("Tournoi de test knock out avec swissPool");
        scenario.init(scenarioTournament);
//        test nom
        Assertions.assertEquals("Tournoi de test knock out avec swissPool", scenarioTournament.getName());
//        test nombre équipe
        Assertions.assertEquals(4, scenarioTournament.getTeams().size());
//        test nombre rounds
        Assertions.assertEquals(2, scenarioTournament.getRounds().size());

//        c'est inversé j'ai pas compris pq mais swissPool est bien 1 dans le debugger
        Assertions.assertEquals("swissPool", scenarioTournament.getRounds().get(0).getType());
        Assertions.assertEquals("knockOut", scenarioTournament.getRounds().get(1).getType());
        scenario.start(scenarioTournament);
//        nombre de rounds
        Assertions.assertEquals(2, scenarioTournament.getRounds().size());
//        nombre de matchs
//        12 matchs dans la pool à 4 équipes
        Assertions.assertEquals(12, scenarioTournament.getRounds().get(0).getSwissPool().getMatches().size());
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
