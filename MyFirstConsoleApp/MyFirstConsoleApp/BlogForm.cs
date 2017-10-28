using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Data.Entity;

namespace MyFirstConsoleApp
{
    public partial class BlogForm : Form
    {
        BlogContext bContext = new BlogContext();

        public BlogForm()
        {
            InitializeComponent();
        }

        private void BlogForm_Load(object sender, EventArgs e)
        {
            bContext.Blogs.Load();
            this.blogBindingSource.DataSource = bContext.Blogs.Local.ToBindingList();
        }

        private void blogBindingNavigatorSaveItem_Click(object sender, EventArgs e)
        {
            bContext.SaveChanges();
        }
    }
}
