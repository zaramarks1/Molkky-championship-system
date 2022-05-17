package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.model.TeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.MatchService;
import com.molkky.molkky.service.PhaseService;
import com.molkky.molkky.service.SetService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.web.servlet.MockMvc;
import type.SetTeamIndex;
import type.TournamentStatus;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.*;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = PhaseController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
 class PhaseControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private PhaseRepository phaseRepository;

    @MockBean
    private TeamRepository teamRepository;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private PhaseService phaseService;

    @Mock
    private Tournament tournament;

    @Test
    void testControllerWithoutUser() throws Exception {
        mockMvc.perform(get("/phase/generate?phase_id=1"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/connexion"));
    }

   /* @Test
    void testGeneratePhaseControllerWithUser() throws Exception {

        UserLogged userLogged = Mockito.mock(UserLogged.class);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        HashMap<Round, List<Match>> response = createRounds();

        this.mockMvc.perform(get("/phase/generate?phase_id=1").sessionAttr("user", userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attribute("round_match", response));

    }*/

    Map<Round, List<Match>> createRounds(){
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
        tournament.setId(1);
        tournament.setNbPlayersPerTeam(2);
        tournament.setVisible(true);
        tournament.setStatus(TournamentStatus.AVAILABLE);
        tournament= tournamentRepository.save(tournament);

        Pool pool = new Pool();
        pool.setId(1);
        pool.setNbSets(1);
        pool.setVictoryValue(2);
        pool.setNbPhase(1);
        pool.setNbPools(2);
        pool.setNbTeamsQualified(4);

        pool.setTournament(tournament);
        pool =  phaseRepository.save(pool);

        List<Phase> phases = new ArrayList<>();
        phases.add(pool);
        tournament.setPhases(phases);
        tournamentRepository.save(tournament);


        for (int i =1; i <= 8; i++){
            Team team = new Team();

            team.setCode("12345");

            team.setName("Team" + i);
            team.setTournament(tournament);


            tournament.getTeams().add(team);

            User player = new User();

            player.setForename("User" + i);

            player =  userRepository.save(player);

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

        return  phaseService.generate("1");

    }

    @Test
    void testPhaseGetMethod() throws Exception{
        //Tournament tournament = new Tournament();
        when(this.tournamentRepository.findById(1)).thenReturn(tournament);
        when(tournament.getNbRounds()).thenReturn(5);

        this.mockMvc.perform(get("/phase/choosePhases?tournamentId=1"))
                .andDo(print())
                .andExpect(model().attributeExists("form"))
                .andExpect(view().name("/phase/choosePhases"));
    }

}
