package com.molkky.molkky.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.molkky.molkky.domain.*;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.AddStaff;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.phase.PhaseListModel;
import com.molkky.molkky.model.phase.PhaseModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import com.molkky.molkky.service.PhaseService;
import com.molkky.molkky.service.TournamentService;
import org.junit.jupiter.api.Assertions;
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
import type.PhaseType;
import type.TournamentStatus;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


@WebMvcTest(value = TournamentController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class TournamentControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private TournamentRepository tournamentRepository;

    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @MockBean
    private NotificationService notificationService;

    @MockBean
    private TournamentService tournamentService;

    @MockBean
    private PhaseService phaseService;

    @MockBean
    private UserRepository userRepository;

    @MockBean
    private TeamRepository teamRepository;

    @Autowired
    private ObjectMapper objectMapper;


    @Mock
    private Tournament tournament;

    @Test
    void testTournamentController() throws Exception {
        mockMvc.perform(get("/tournament/create/"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("tournament/create"));

        when(tournamentService.create(any(TournamentModel.class))).thenReturn(this.tournament);
        when(this.tournament.getId()).thenReturn(5);

        mockMvc.perform(post("/tournament/create/")
                        .param("date", "2020-06-01")
                        .param("cutOffDate", "2020-03-01")
                        .flashAttr("tournament", new TournamentModel()))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/phase/choosePhases?tournamentId=5"));
        ;
        verify(tournamentService, times(1)).create(any(TournamentModel.class));
    }

    @Test
    void testTournamentViewPostLaunchWithAdmin() throws Exception {
        Tournament tournament = new Tournament();
        tournament.setId(1);
        tournament.setTeams(Arrays.asList(new Team(), new Team()));

        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setTournamentRoleId(1);
        userLogged.setRole(UserRole.ADM);
        userLogged.setTournament(tournament);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        when(this.tournamentRepository.findById(1)).thenReturn(tournament);
        when(userLogged.getTournament()).thenReturn(tournament);
        when(userLogged.getRole()).thenReturn(UserRole.ADM);
        when(tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);

        mockMvc.perform(get("/tournament/view")
                        .sessionAttr("user", userLogged)
                        .param("tournamentId", tournament.getId().toString()))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/view"));

        verify(this.tournamentRepository, times(1)).findById(anyInt());
    }

    @Test
    void testTournamentViewPostLaunchWithoutAdmin() throws Exception {
        Tournament tournament = new Tournament();
        tournament.setId(1);
        tournament.setTeams(Arrays.asList(new Team(), new Team()));

        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setTournamentRoleId(1);
        userLogged.setRole(UserRole.PLAYER);
        userLogged.setTournament(tournament);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        List<Phase> phases = new ArrayList<>();
        PhaseListModel phaseListModels = new PhaseListModel();

        PhaseModel phaseModelPool = new PhaseModel();
        phaseModelPool.setPhaseType(PhaseType.POOL);
        phaseModelPool.setTournament(tournament.getId());
        phaseModelPool.setHourPhaseStart("");
        phaseModelPool.setTimePhase("");
        phaseListModels.add(phaseModelPool);
        phases.add(new Pool(phaseModelPool, tournament));

        PhaseModel phaseModelSimpleGame = new PhaseModel();
        phaseModelSimpleGame.setPhaseType(PhaseType.SIMPLEGAME);
        phaseModelSimpleGame.setTournament(tournament.getId());
        phaseModelSimpleGame.setHourPhaseStart("");
        phaseModelSimpleGame.setTimePhase("");
        phaseListModels.add(phaseModelSimpleGame);
        phases.add(new SimpleGame(phaseModelSimpleGame, tournament));

        PhaseModel phaseModelKnockOut = new PhaseModel();
        phaseModelKnockOut.setPhaseType(PhaseType.KNOCKOUT);
        phaseModelKnockOut.setTournament(tournament.getId());
        phaseModelKnockOut.setHourPhaseStart("");
        phaseModelKnockOut.setTimePhase("");
        phaseListModels.add(phaseModelKnockOut);
        phases.add(new Knockout(phaseModelKnockOut, tournament));

        PhaseModel phaseModelSwissPool = new PhaseModel();
        phaseModelSwissPool.setPhaseType(PhaseType.SWISSPOOL);
        phaseModelSwissPool.setTournament(tournament.getId());
        phaseModelSwissPool.setHourPhaseStart("");
        phaseModelSwissPool.setTimePhase("");
        phaseListModels.add(phaseModelSwissPool);
        phases.add(new SwissPool(phaseModelSwissPool, tournament));

        PhaseModel phaseModelFinnish = new PhaseModel();
        phaseModelFinnish.setPhaseType(PhaseType.FINNISH);
        phaseModelFinnish.setTournament(tournament.getId());
        phaseModelFinnish.setHourPhaseStart("");
        phaseModelFinnish.setTimePhase("");
        phaseListModels.add(phaseModelFinnish);
        phases.add(new Finnish(phaseModelFinnish, tournament));

        tournament.setPhases(phases);

        when(this.tournamentRepository.findById(1)).thenReturn(tournament);
        when(userLogged.getTournament()).thenReturn(tournament);
        when(userLogged.getRole()).thenReturn(UserRole.PLAYER);
        when(tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);
        //when(tournament.getPhases()).thenReturn(phases);

        mockMvc.perform(get("/tournament/view")
                        .sessionAttr("user", userLogged)
                        .param("tournamentId", tournament.getId().toString()))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/view"));

        verify(this.tournamentRepository, times(1)).findById(anyInt());

        Assertions.assertEquals(5, tournament.getPhases().size());
    }

    @Test
    void testAllTournament() throws Exception{
        mockMvc.perform(get("/tournament/allTournament"))
                .andExpect(status().is2xxSuccessful())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/allTournament"));

        mockMvc.perform(get("/tournament/TournamentOpen"))
                .andExpect(status().is2xxSuccessful())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/allTournament"));

        mockMvc.perform(get("/tournament/TournamentClose"))
                .andExpect(status().is2xxSuccessful())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/allTournament"));

        mockMvc.perform(get("/tournament/TournamentInProgress"))
                .andExpect(status().is2xxSuccessful())
                .andExpect(model().attributeExists("tournament"))
                .andExpect(view().name("/tournament/allTournament"));

        mockMvc.perform(post("/tournament/allTournament"))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/create"));

        mockMvc.perform(post("/tournament/inscription"))
                .andDo(print())
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/team/create"));
    }

    @Test
    void testResults() throws Exception {
        Tournament tournament = new Tournament();
        tournament.setId(289989);
        tournament.setNbPlayersPerTeam(1);
        tournament.setFinished(true);
        Team team = new Team();
        team.setEliminated(false);
        User user = new User();
        user.setForename("Paco");
        user.setSurname("Bousson");
        UserTournamentRole userTournamentRole = new UserTournamentRole();
        List<UserTournamentRole> users = new ArrayList<>();
        users.add(userTournamentRole);
        List<Team> teams = new ArrayList<>();
        teams.add(team);

        userTournamentRole.setUser(user);
        team.setUserTournamentRoles(users);
        tournament.setTeams(teams);
        tournament.setStatus(TournamentStatus.CLOSED);

        Mockito.when(tournamentRepository.findById(Mockito.anyInt())).thenReturn(tournament);
        Mockito.when(tournamentService.getWinners(tournament)).thenReturn(teams);

        mockMvc.perform(get("/tournament/results")
                .param("tournamentId", "289989"))
                .andExpect(status().isOk())
                .andExpect(view().name("/tournament/results"));
    }

    @Test
    void testTournamentOnGoing() throws Exception{
        mockMvc.perform(get("/tournament/tournamentOnGoing"))
                .andExpect(status().is2xxSuccessful())
                .andExpect(view().name("/tournament/tournamentOnGoing"));
    }

    @Test
    void testTournamentValidatePresence() throws Exception{
        Team team = new Team();
        team.setId(1);
        Tournament tournoi = new Tournament();
        tournoi.setId(1);
        tournoi.setTeams(Arrays.asList(team));
        when(this.teamRepository.findById(1)).thenReturn(team);
        mockMvc.perform(post("/tournament/validatePresence")
                .param("tournamentId", tournoi.getId().toString())
                .param("teamId",team.getId().toString()))
                .andExpect(view().name("redirect:/tournament/view?tournamentId="+tournoi.getId()));
    }

    @Test
    void testAddStaffToTournament() throws Exception{
        int id = (int) (Math.random() * 100000);
        Tournament tournament = new Tournament();
        tournament.setId(id);

        List<AddStaff> staffList = new ArrayList<>();
        staffList.add(new AddStaff());

        String staffCounter = String.valueOf(staffList.size());

        mockMvc.perform(post("/tournament/addStaff")
                        .param("tournamentId", tournament.getId().toString())
                        .param("staffCount", staffCounter))
                .andExpect(status().is2xxSuccessful())
                .andExpect(model().attributeExists("isDiffMail"))
                .andExpect(model().attributeExists("staffList"))
                .andExpect(view().name("/tournament/addStaff"));
    }

    @Test
    void testSetVisibleTournament() throws Exception{
        int id = (int) (Math.random() * 100000);
        Tournament tournament = new Tournament();
        tournament.setId(id);

        Mockito.when(tournamentRepository.findById(id)).thenReturn(tournament);

        mockMvc.perform(post("/tournament/setVisible")
                        .param("tournamentId", tournament.getId().toString()))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/view?tournamentId=" + tournament.getId()));
    }

    @Test
    void testPublishTournament() throws Exception {
        int id = (int) (Math.random() * 100000);
        Tournament tournament = new Tournament();
        tournament.setId(id);
        List<Phase> phases = new ArrayList<>();
        Phase phase = new Phase();
        phase.setId(242490);
        phases.add(phase);
        tournament.setPhases(phases);
        Map<Round, List<Match>> results = new HashMap<>();
        List<Match> matches = new ArrayList<>();
        matches.add(new Match());
        results.put(new Round(), matches);

        Mockito.when(tournamentRepository.findById(id)).thenReturn(tournament);
        Mockito.when(tournamentRepository.save(Mockito.any(Tournament.class))).thenAnswer(i -> i.getArguments()[0]);
        Mockito.when(phaseService.generate("242490")).thenReturn(results);

        mockMvc.perform(post("/tournament/publish")
                        .param("tournamentId", tournament.getId().toString()))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/view?tournamentId=" + tournament.getId()));

        Assertions.assertEquals("INPROGRESS", tournament.getStatus().toString());
        Assertions.assertEquals(1, tournament.getIndexPhase());

        Mockito.verify(tournamentRepository, Mockito.times(1)).save(tournament);
    }

    @Test
    void testResultsPost() throws Exception {
        int id = (int) (Math.random() * 100000);
        Tournament tournoi = new Tournament();
        tournoi.setId(id);
        mockMvc.perform(post("/tournament/results")
                        .param("tournamentId", tournoi.getId().toString()))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/tournament/results?tournamentId=" + tournoi.getId()));
    }
}
