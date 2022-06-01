package com.molkky.molkky.controllers;

import com.molkky.molkky.service.TournamentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import java.time.LocalDateTime;

@Configuration
@EnableScheduling
public class ScheduleController {

    @Autowired
    private TournamentService tournamentService;

    //@Scheduled(cron = "[Seconds] [Minutes] [Hours] [Day of month] [Month] [Day of week] [Year]")

    @Scheduled(cron = "0 19 16 * * ?")
    public void scheduleFixedDelayTask() throws Exception {
        tournamentService.isMinimumTeamsBeforeDate();
        tournamentService.registerClosedForTournament();

        System.out.println("Fixed delay task - " + LocalDateTime.now().toString());
        System.out.println("OK SCHEDULE");
    }
}
