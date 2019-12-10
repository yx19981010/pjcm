package com.samsph.pjcm.service;

import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.query.JournalQuery;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.test.context.junit4.SpringRunner;

import javax.annotation.Resource;

import static com.samsph.pjcm.config.DevUserId.ADMIN_ID;
import static org.hamcrest.CoreMatchers.is;


@SpringBootTest
@RunWith(SpringRunner.class)
class JournalServiceTest {

    @Resource
    private JournalService journalService;

    @Resource
    private JournalRepository journalRepository;

    private JournalQuery saveQuery = new JournalQuery(null, 2019, 12, 1, 1);

    @AfterEach
    void tearDown() {
        journalRepository.deleteAll();
    }

    @Test
    void saveJournal() {
        Journal journal = journalService.saveJournal(saveQuery, ADMIN_ID);

        Assert.assertThat(journal.getMonth(), is(12));
        Assert.assertThat(journal.getYear(), is(2019));
        Assert.assertThat(journal.getVolume(), is(1));
        Assert.assertThat(journal.getNumber(), is(1));
        Assert.assertThat(journal.getTotal(), is(0));
        Assert.assertThat(journal.getCreate_by_uid(), is(ADMIN_ID));

        try {
            journalService.saveJournal(saveQuery, ADMIN_ID);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("期刊已存在"));
        }
    }

    @Test
    void getJournalById() {
        Journal journal = journalService.saveJournal(saveQuery, ADMIN_ID);

        Journal journal1 = journalService.getJournal(journal.getId());
        Assert.assertThat(journal1.getMonth(), is(12));
        Assert.assertThat(journal1.getYear(), is(2019));
        Assert.assertThat(journal1.getVolume(), is(1));
        Assert.assertThat(journal1.getNumber(), is(1));
        Assert.assertThat(journal1.getTotal(), is(0));
        Assert.assertThat(journal1.getCreate_by_uid(), is(ADMIN_ID));

        try {
            journalService.getJournal(0);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("期刊未找到"));
        }
    }

    @Test
    void getJournalByYMVN() {
        Journal journal = journalService.saveJournal(saveQuery, ADMIN_ID);

        Journal journal1 = journalService.getJournal(2019, 12, 1, 1);

        Assert.assertThat(journal1.getMonth(), is(12));
        Assert.assertThat(journal1.getYear(), is(2019));
        Assert.assertThat(journal1.getVolume(), is(1));
        Assert.assertThat(journal1.getNumber(), is(1));
        Assert.assertThat(journal1.getTotal(), is(0));
        Assert.assertThat(journal1.getCreate_by_uid(), is(ADMIN_ID));

        try {
            journalService.getJournal(9999, 12, 1, 1);
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("期刊未找到"));
        }
    }

    @Test
    void getAll() {
        JournalQuery saveQuery2 = new JournalQuery(null, 2018, 11, 2, 2);
        journalService.saveJournal(saveQuery, ADMIN_ID);
        journalService.saveJournal(saveQuery2, ADMIN_ID);

        Page<Journal> journalPage = journalService.getAll(2, 1, true);

        Assert.assertThat(journalPage.getNumber(), is(1));
        Assert.assertThat(journalPage.getSize(),is(1));
        Assert.assertThat(journalPage.getTotalPages(), is(2));
        Assert.assertThat(journalPage.getTotalElements(), is(2L));
        Assert.assertThat(journalPage.getNumberOfElements(),is(1));

        Journal journal = journalPage.getContent().get(0);
        Assert.assertThat(journal.getMonth(), is(11));
        Assert.assertThat(journal.getYear(), is(2018));
        Assert.assertThat(journal.getVolume(), is(2));
        Assert.assertThat(journal.getNumber(), is(2));
        Assert.assertThat(journal.getTotal(), is(0));
        Assert.assertThat(journal.getCreate_by_uid(), is(ADMIN_ID));

        journalPage =  journalService.getAll(2, 4, true);

        Assert.assertThat(journalPage.getNumber(), is(1));
        Assert.assertThat(journalPage.getSize(),is(4));
        Assert.assertThat(journalPage.getTotalPages(), is(1));
        Assert.assertThat(journalPage.getTotalElements(), is(2L));
        Assert.assertThat(journalPage.getNumberOfElements(),is(0));
    }


    @Test
    void deleteJournal() {
        Journal journal = journalService.saveJournal(saveQuery, ADMIN_ID);

        journalService.deleteJournal(journal.getId());

        try {
            journalService.getJournal(journal.getId());
        } catch (CustomException ex) {
            Assert.assertThat(ex.getCode(), is(400));
            Assert.assertThat(ex.getMessage(), is("期刊未找到"));
        }
    }

    @Test
    void updateJournal() {
        Journal journal = journalService.saveJournal(saveQuery, ADMIN_ID);
        int id = journal.getId();
        journal = journalService.getJournal(id);

        journal.setVolume(2);
        journal.setNumber(2);
        journalService.updateJournal(journal);
        journal = journalService.getJournal(id);

        Assert.assertThat(journal.getMonth(), is(12));
        Assert.assertThat(journal.getYear(), is(2019));
        Assert.assertThat(journal.getVolume(), is(2));
        Assert.assertThat(journal.getNumber(), is(2));
        Assert.assertThat(journal.getTotal(), is(0));
        Assert.assertThat(journal.getCreate_by_uid(), is(ADMIN_ID));
    }
}