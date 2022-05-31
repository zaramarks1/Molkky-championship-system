package com.molkky.molkky.controllers;

import com.molkky.molkky.model.TeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.*;
import com.molkky.molkky.utility.StringUtilities;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(value = SearchController.class, excludeAutoConfiguration = {SecurityAutoConfiguration.class})
@ExtendWith(MockitoExtension.class)
class SearchControllerTest {
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
    @MockBean
    private SearchService searchService;
    @Autowired
    private SearchController searchController;

    @Test
    void emptyTermTest() throws Exception {
        mockMvc.perform(get("/search/search"))
                .andExpect(status().is4xxClientError());
    }

    @Test
    void TermTest() throws Exception {
//        given
        String randomName = StringUtilities.createCode(80);
        when(searchService.searchTournaments(anyString())).thenReturn(List.of(new TournamentModel()));
        when(searchService.searchUsers(anyString())).thenReturn(List.of(new UserModel()));
        when(searchService.searchTeams(anyString())).thenReturn(List.of(new TeamModel()));
//        when
        mockMvc.perform(get("/search/search?term="+randomName))
                .andExpect(status().is2xxSuccessful());
//        then
        verify(searchService, times(1)).searchTournaments(randomName, 5);
        verify(searchService, times(1)).searchTeams(randomName, 5);
        verify(searchService, times(1)).searchUsers(randomName, 5);
    }
}
