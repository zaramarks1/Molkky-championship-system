package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.model.*;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.web.servlet.MockMvc;
import type.SetTeamIndex;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(value = MatchController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class MatchControllerTest {
    @Autowired
    private MockMvc mockMvc;
    @MockBean
    private MatchRepository matchRepository;
    @MockBean
    private SetService setService;
    @MockBean
    private UserService userService;
    @MockBean
    private MatchService matchService;
    @MockBean
    private CourtRepository courtRepository;
    @MockBean
    private UserRepository userRepository;
    @MockBean
    private CourtService courtService;
    @MockBean
    private TeamRepository teamRepository;
    @MockBean
    private NotificationService notificationService;
    @MockBean
    private UserTournamentRoleRepository userTournamentRoleRepository;
    @Autowired
    private MatchController matchController;

    @Test
    void testMatchWithoutUser() throws Exception {
        Match match = createMatch();
        when(matchRepository.findById(1)).thenReturn(match);
        mockMvc.perform(get("/matches/match?match_id=1"))
                .andExpect(status().isOk())
                .andExpect(model().attribute("match", MatchService.getMatchModelFromEntity(match)))
                .andExpect(model().attribute("teams", TeamModel.createTeamModels(match.getTeams())))
                //.andExpect(model().attribute("court", new CourtModel(match.getCourt())))
                .andExpect(model().attribute("tournament", new TournamentModel(match.getRound().getPhase().getTournament())))
                .andExpect(model().attribute("sets", SetService.createSetModels(match.getSets())))
                .andExpect(model().attribute("setTeamIndex", SetTeamIndex.OUTSIDER))
                .andExpect(view().name("/match/match"));
    }

    @Test
    void testMatchWithPlayerInMatch() throws Exception {
        UserLogged userLogged = Mockito.mock(UserLogged.class);
        Tournament t = new Tournament();
        t.setId(1);
        when(userLogged.getTournamentRoleId()).thenReturn(1);
        when(userLogged.getTournament()).thenReturn(t);
        Match match = createMatch();
        when(matchRepository.findById(1)).thenReturn(match);

        UserTournamentRole userTournamentRole = Mockito.mock(UserTournamentRole.class);
        userTournamentRole.setId(1);
        userTournamentRole.setRole(UserRole.PLAYER);
        userTournamentRole.setIsRegistered(true);
        when(userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole);

        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserTournamentRoleModel.class))).thenReturn(SetTeamIndex.TEAM1);

        this.mockMvc.perform(get("/matches/match?match_id=1").sessionAttr("user", userLogged)
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attribute("match", MatchService.getMatchModelFromEntity(match)))
                .andExpect(model().attribute("teams", TeamModel.createTeamModels(match.getTeams())))
                //.andExpect(model().attribute("court", new CourtModel(match.getCourt())))
                .andExpect(model().attribute("tournament", new TournamentModel(match.getRound().getPhase().getTournament())))
                .andExpect(model().attribute("sets", SetService.createSetModels(match.getSets())))
                .andExpect(model().attribute("setTeamIndex", SetTeamIndex.TEAM1))
                .andExpect(model().attribute("user", userLogged))
                .andExpect(view().name("/match/match"));
    }

    @Test
    void testMatchWithoutPlayerInMatch() throws Exception {
        Tournament t = new Tournament();
        t.setId(1);

        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setTournamentRoleId(1);
        userLogged.setRole(UserRole.PLAYER);
        userLogged.setTournament(t);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        when(userLogged.getTournamentRoleId()).thenReturn(1);
        Match match = createMatch();
        when(matchRepository.findById(1)).thenReturn(match);

        UserTournamentRole userTournamentRole = Mockito.mock(UserTournamentRole.class);
        userTournamentRole.setId(1);
        userTournamentRole.setRole(UserRole.PLAYER);
        userTournamentRole.setIsRegistered(true);
        when(userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole);

        when(matchService.getUserTeamIndex(any(MatchModel.class), any(UserTournamentRoleModel.class))).thenReturn(SetTeamIndex.TEAM1);
        when(matchService.isUserInMatch(MatchService.getMatchModelFromEntity(match), UserService.createUserModelFromUserLogged(userLogged))).thenReturn(false);
        when(userLogged.getRole()).thenReturn(UserRole.PLAYER);

        this.mockMvc.perform(get("/matches/match?match_id=1").sessionAttr("user", userLogged)
                        .sessionAttr("user",userLogged))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:/"));
    }

    @Test
    void testMatchesWithPlayer() throws Exception {
        Tournament tournament = new Tournament();
        tournament.setId((int)Math.random() * 10000);

        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setId(399992);
        userLogged.setTournamentRoleId(1);
        userLogged.setRole(UserRole.PLAYER);
        userLogged.setTournament(tournament);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        TeamModel teamModel = new TeamModel();
        Team team = new Team();

        when(userLogged.getRole()).thenReturn(UserRole.PLAYER);
        when(userLogged.getTeam()).thenReturn(teamModel);
        when(teamRepository.findById(Mockito.anyInt())).thenReturn(team);

        this.mockMvc.perform(get("/match/allMatches")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));

        this.mockMvc.perform(get("/match/inProgressMatches")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));

        this.mockMvc.perform(get("/match/finishedMatches")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));
    }

    @Test
    void testValidateMatches() throws Exception{
        Tournament tournament = new Tournament();
        tournament.setId((int)Math.random() * 10000);

        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setId(399992);
        userLogged.setTournamentRoleId(1);
        userLogged.setRole(UserRole.PLAYER);
        userLogged.setTournament(tournament);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        Round round = new Round();
        Phase phase = new Phase();
        phase.setNbSets(2);
        round.setPhase(phase);
        List<Team> teams = new ArrayList<>();
        teams.add(new Team());
        teams.add(new Team());
        List<Match> matchesStaff = new ArrayList<>();
        Match match1 = new Match();
        Match match2 = new Match();
        List<Set> sets = new ArrayList<>();
        Set set1 = new Set();
        Set set2 = new Set();
        set1.setScore1Team1(40);
        set1.setScore1Team2(50);
        sets.add(set1);
        sets.add(set2);
        match1.setSets(sets);
        match1.setTeams(teams);
        match1.setRound(round);
        match2.setSets(sets);
        match2.setTeams(teams);
        match2.setRound(round);
        matchesStaff.add(match1);
        matchesStaff.add(match2);
        User staff = new User();

        when(userRepository.findById(Mockito.anyInt())).thenReturn(staff);
        when(matchRepository.findMatchAttributedToStaff(tournament, staff)).thenReturn(matchesStaff);
        when(matchRepository.findMatchAttributedToStaff(Mockito.any(Tournament.class), Mockito.any(User.class))).thenReturn(matchesStaff);
        when(userLogged.getTournament()).thenReturn(tournament);

        this.mockMvc.perform(get("/match/validateMatch")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));
    }

    @Test
    void testMatchesWithStaff() throws Exception {
        Tournament tournament = new Tournament();
        tournament.setId((int)Math.random() * 10000);

        UserLogged userLogged = mock(UserLogged.class);
        userLogged.setId(399992);
        userLogged.setTournamentRoleId(1);
        userLogged.setRole(UserRole.STAFF);
        userLogged.setTournament(tournament);
        HttpSession session = new MockHttpSession(null, "user");
        session.setAttribute("user", userLogged);

        User userStaff = new User();

        when(userLogged.getRole()).thenReturn(UserRole.STAFF);
        when(userLogged.getTournament()).thenReturn(tournament);
        when(userRepository.findById(Mockito.anyInt())).thenReturn(userStaff);

        this.mockMvc.perform(get("/match/allMatches")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));

        this.mockMvc.perform(get("/match/inProgressMatches")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));

        this.mockMvc.perform(get("/match/finishedMatches")
                        .sessionAttr("user",userLogged))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("matches"))
                .andExpect(view().name("match/allMatches"));
    }


    @Test
    void updateMatchCourtTest() throws Exception {
//        given
        UserLogged userLogged = Mockito.mock(UserLogged.class);
        userLogged.setRole(UserRole.STAFF);
        userLogged.setTournamentRoleId(1);
        when(userLogged.getRole()).thenReturn(UserRole.STAFF);
        UserTournamentRole userTournamentRole = Mockito.mock(UserTournamentRole.class);
        userTournamentRole.setId(1);
        userTournamentRole.setRole(UserRole.STAFF);
        userTournamentRole.setIsRegistered(true);
        when(userTournamentRoleRepository.findById(anyInt())).thenReturn(userTournamentRole);
        Match match = createMatch();
        when(matchRepository.findById(1)).thenReturn(match);
        match.setId(1);
        Court newCourt = new Court(true, "New court Test");
        newCourt.setId(800);
//        when

//        then
        this.mockMvc.perform(post("/match/updateMatchCourt")
                        .sessionAttr("user", userLogged)
                        .param("id", newCourt.getId().toString())
                        .param("matchId", match.getId().toString())
                )
                .andExpect(status().is3xxRedirection());
        verify(matchService, times(1)).setCourt(any(MatchModel.class), any(CourtModel.class));
    }

    Match createMatch() {
        Match match = new Match();
        Court court = new Court();
        court.setName("court");
        match.setCourt(court);
        Team team1 = new Team();
        team1.setId(1);
        Team team2 = new Team();
        team2.setId(2);
        match.setTeams(Arrays.asList(team1, team2));
        Tournament tournament = new Tournament();
        tournament.setName("tournament");
        tournament.setId(1);
        tournament.setDate(Date.from(new Date().toInstant()));
        Phase phase = new Phase();
        Round round = new Round();
        round.setTournament(tournament);
        round.setPhase(phase);
        phase.setRounds(List.of(round));
        phase.setTournament(tournament);
        match.setRound(round);
        Set set = new Set();
        set.setMatch(match);
        match.setSets(List.of(set));
        return match;
    }
}
