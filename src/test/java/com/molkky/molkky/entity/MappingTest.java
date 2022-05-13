package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import javax.transaction.Transactional;

@SpringBootTest(classes = MolkkyApplication.class)
 class MappingTest {

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private TeamRepository teamRepository;

    @Test
    void tournamentTeamMapping(){

        Tournament tournament = new Tournament();
        tournament.setName("test mapping");
        tournament = tournamentRepository.save(tournament);
        Team t = new Team();
        t.setName("team test");
        t.setTournament(tournament);

        t = teamRepository.save(t);
        tournament.getTeams().add(t);

        tournament = tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getTeams().size(), "Tournament should have 1 team");

        Team team = tournament.getTeams().get(0);

        Assertions.assertEquals(tournament, team.getTournament(), "Tournament  not found");

    }

    @Test
    void tournamentTeamMapping3(){

        Tournament tournament = new Tournament();
        Team t = new Team();
        t.setName("team test 3");
        tournament.setName("test mapping 3");
        t.setTournament(tournament);
        tournament.getTeams().add(t);

        tournament = tournamentRepository.save(tournament);

        Assertions.assertEquals(1, tournament.getTeams().size(), "Tournament should have 1 team");

        Team team = tournament.getTeams().get(0);

        Assertions.assertEquals(tournament.getId(), team.getTournament().getId(), "Tournament  not found");

        team = teamRepository.findById(team.getId());

        Assertions.assertEquals(tournament.getId(), team.getTournament().getId(), "Tournament  not found");

    }



}
