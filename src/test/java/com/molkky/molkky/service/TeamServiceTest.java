package com.molkky.molkky.service;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class TeamServiceTest {

        @Autowired
        private TeamService teamService;

        @Autowired
        private TeamRepository teamRepository;

        @Mock
        private CreateTeamModel teamModel;

        @Mock
        private TournamentRepository tournamentRepository;

        @Test
        public void testCreateTeam(){
            Integer idTournament = 1;
            String teamName = "TeamMock"+Math.floor((Math.random() * 1000));

            when(teamModel.getTournament()).thenReturn(1);
            when(tournamentRepository.findById(idTournament)).thenReturn(new Tournament());
            when(teamModel.getName()).thenReturn(teamName);

            teamService.create(teamModel);

            Team team = teamRepository.findByName(teamName);

        }

    @Test
    public void testCreateCodeLength(){
        String code = teamService.createCode(10);
        Assert.assertEquals("Length not good",10,code.length());
    }
}
